(defpackage #:dex-edn
  (:use #:common-lisp #:alexandria)
  (:export #:decode
	   #:encode

	   #:read-tagged
	   #:prn

	   #:edn-set
	   #:edn-uuid
	   #:edn-inst

	   #:make-set
	   #:make-uuid
	   #:make-inst

	   #:value

	   #:decode-error
	   #:syntax-error))
