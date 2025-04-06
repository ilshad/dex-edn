(defsystem "dex-edn"
  :description "EDN (Extensible Data Notation) parser and emitter for Common Lisp"
  :author "Ilshad Khabibullin <astoon.net@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :homepage "https://github.com/ilshad/dex-edn"
  :bug-tracker "https://github.com/ilshad/dex-edn/issues"
  :source-control (:git "git@github.com:ilshad/dex-edn.git")
  :in-order-to ((test-op (test-op "dex-edn/tests")))
  :serial t
  :depends-on ("alexandria")
  :components ((:file "package")
	       (:file "wrappers")
	       (:file "decode")
	       (:file "encode")))

(defsystem "dex-edn/tests"
  :depends-on ("dex-edn" "fiveam")
  :perform (test-op (o c)
		    (symbol-call :fiveam :run! (find-symbol* :tests :dex-edn/tests)))
  :components ((:module "tests"
		:components ((:file "tests")))))
