(defpackage #:dex-edn
  (:use #:common-lisp #:alexandria)
  (:export #:decode
	   #:encode

	   #:prn
	   #:read-tagged

	   #:decode-error
	   #:syntax-error
	   #:unknown-symbol-error))
