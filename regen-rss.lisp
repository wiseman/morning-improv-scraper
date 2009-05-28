(setf *load-verbose* NIL)

(defun ignore-warning (condition)
  (declare (ignore condition))
  (muffle-warning))

#+sbcl
(require :asdf)

#+franz
(unless (find-package "ASDF")
  (load "/usr/local/bin/acl70/siteinit.cl"))

(push "/Users/wiseman/src/mi-rss-scraper/" asdf:*central-registry*)

(handler-bind ((warning #'ignore-warning))
  (asdf:operate 'asdf:load-op :mi-rss :verbose NIL))

(fetch-current-morning-improv)

(quit)
