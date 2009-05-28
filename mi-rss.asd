(in-package :asdf)

(defsystem :mi-rss
    :author "John Wiseman <jjwiseman@yahoo.com>"
    :version "0.1"
    :maintainer "John Wiseman <jjwiseman@yahoo.com>"
    :licence "MIT"
    :depends-on (:cl-ppcre
		 :araneida
		 :net-telent-date
		 :xmls
		 :trivial-browser)
    :components ((:file "xmls-patch")
		 (:file "mi-rss"
			:depends-on ("xmls-patch"))))
