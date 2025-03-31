(defpackage #:dex-edn
  (:use #:common-lisp #:alexandria)
  (:export #:decode
	   #:encode

	   #:prn
	   #:read-tagged
	   #:edn-set
	   #:make-set
	   #:value

	   #:decode-error
	   #:syntax-error
	   #:unknown-symbol-error))
