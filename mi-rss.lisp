(defparameter *post-delimiter-re* (cl-ppcre:create-scanner "<b>[0-9]+/[0-9]+.*?</b>"))


(defun chunk-mi-stream (in)
  (let ((s (preprocess-mi-stream in)))
    (with-input-from-string (in s)
      (let ((chunks '())
	    (chunk '()))
	(labels ((new-chunk ()
		   (push (apply #'concatenate 'string (reverse chunk)) chunks)
		   (setf chunk '()))
		 (add-to-chunk (line)
		   (push line chunk)))
	  (let ((state :out))
	    (do ((line (read-line in nil :eof) (read-line in nil :eof)))
		((eq line :eof))
	      (ecase state
		((:out)
		 (when (cl-ppcre:scan *post-delimiter-re* line)
		   (setf state :in)
		   (add-to-chunk line)))
		((:in)
		 (if (cl-ppcre:scan *post-delimiter-re* line)
		     (progn
		       (new-chunk)
		       (add-to-chunk line))
		     (add-to-chunk line)))))))
	(reverse chunks)))))


(defun preprocess-mi-stream (stream)
  (cleanup-post-delimiters (remove-newlines stream)))

(defun remove-newlines (stream)
  (with-output-to-string (s)
    (do ((line (read-line stream nil :eof) (read-line stream nil :eof)))
	((eq line :eof))
      (format s "~A " line))))


(defparameter *cleanup-post-search-re* (cl-ppcre:create-scanner "<b>\\W*?([0-9]+/[0-9]+.*?)</b>"))

;; For some reason if we try to use a scanner here we get an error
;; when we go to do the replacing.
(defparameter *cleanup-post-replace-re* (format nil "~%<b>\\{1}</b>~%"))

(defun cleanup-post-delimiters (string)
  (cl-ppcre:regex-replace-all *cleanup-post-search-re*
			      string
			      *cleanup-post-replace-re*))


(defparameter *post-date-re* (cl-ppcre:create-scanner "<b>([0-9]+/[0-9]+.*?):.*?</b>"))
(defun post-date (post)
  (multiple-value-bind (a b starts ends)
      (cl-ppcre:scan *post-date-re* post)
    (declare (ignore a b))
    (let ((groups '()))
      (dotimes (i (length starts) (car groups))
	(push (subseq post (elt starts i) (elt ends i))
	      groups)))))


(defun split-file (f)
  (with-open-file (in f :direction :input)
    (chunk-mi-stream in)))


(defun mi-posts-to-rss (chunks stream)
  (let ((rss `("rss" (("version" "0.91"))
	        ("channel" ()
		  ("title" () "Scott McCloud's Morning Improv")
		  ("description" () "One hour each day, whatever comes into my head.")
		  ("link" () "http://scottmccloud.com/comics/mi/mi.html")
		  ,@(mapcar #'mi-post-to-rss chunks)))))
    (format stream "~A" (xmls:toxml rss :with-namespace NIL))))


(defparameter *post-link-re* (cl-ppcre:create-scanner "[^0-9]"))

(defun post-link (post)
  (let ((date (post-date post)))
    (let ((cooked-date (cl-ppcre:regex-replace-all *post-link-re* date "-")))
      (format nil "http://scottmccloud.com/comics/mi/mi.html#~A" cooked-date))))


(defun mi-post-to-rss (chunk)
  `("item" ()
     ("title" () ,(post-title chunk))
     ("link" () ,(post-link chunk))
     ("description" ()
		    ,(format nil "~A~&" (strip-fonts chunk)))))


(defun strip-html (string)
  (let ((in-tag-p NIL))
    (with-output-to-string (s)
      (dotimes (i (length string))
	(let ((char (char string i)))
	  (cond ((and (not in-tag-p) (not (eql char #\<)))
		 ;; Text
		 (write-char char s))
		((and (not in-tag-p) (eql char #\<))
		 ;; Open tag
		 (setf in-tag-p T))
		((and in-tag-p (not (eql char #\>)))
		 ;; Tag text
		 )
		((and in-tag-p (eql char #\>))
		 ;; Close tag
		 (setf in-tag-p NIL))))))))


(defparameter *font-end-search-re* (cl-ppcre:create-scanner "</font>"))
(defparameter *font-begin-search-re* (cl-ppcre:create-scanner "<font.*?>"))
(defun strip-fonts (string)
  (cl-ppcre:regex-replace-all *font-end-search-re*
			      (cl-ppcre:regex-replace-all *font-begin-search-re* string "")
			      ""))

(defun ending-punctuation-p (char)
  (member char '(#\. #\! #\; #\? #\:)))

(defun title-end-word-p (word)
  (let ((end-pos (position-if #'ending-punctuation-p word)))
    (and end-pos
	 (= end-pos (- (length word) 1)))))

(defun post-title (chunk &key (max-words 15) (min-words 4))
  (let* ((words (split-words (strip-html chunk))))
    (let ((title-words '()))
      (labels ((accum-words (words)
		 (let ((max-additional-words (max (- max-words (length title-words)) 0)))
		   (setf title-words (append title-words (subseq words
								 0
								 (min max-additional-words
								      (length words)))))))
	       (next-title-bit (start num-words)
		 (if (and (< num-words min-words)
			  (< start (length words)))
		     (let ((end-pos (position-if #'title-end-word-p words :start start)))
		       (if end-pos
			   (let ((subwords (subseq words start (+ end-pos 1))))
			     (accum-words subwords)
			     (next-title-bit (+ end-pos 1) (+ num-words (length subwords))))
			   (accum-words (subseq words start)))))))
	(next-title-bit 0 0))
      (reduce #'(lambda (a b) (format nil "~A ~A" a b)) title-words))))


(defun split-words (text)
  (let ((words (cl-ppcre:split "\\s+" text)))
    (remove-if #'(lambda (w) (= (length w) 0)) words)))



(defparameter *morning-improv-url* "http://scottmccloud.com/comics/mi/mi.html")

(defun fetch-current-morning-improv ()
  (let ((browser (make-instance 'trivial-browser:browser
				:user-agent "Morning Improv RSS scraper; jjwiseman@yahoo.com"
				:debug-log-stream *standard-output*)))
    (let ((page (trivial-browser:get-url browser
					 *morning-improv-url*)))
      (with-input-from-string (in page)
	(let ((posts (chunk-mi-stream in)))
	  (with-open-file (out "/Users/wiseman/Sites/morning-improv-feed/mi.xml"
			       :direction :output
			       :if-exists :supersede)
	    (mi-posts-to-rss posts out)))))))

      