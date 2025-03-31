(defsystem "dex-edn"
  :description "EDN (Extensible Data Notation) parser and emitter for Common Lisp"
  :author "Ilshad Khabibullin <astoon.net@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :homepage "https://github.com/ilshad/dex-edn"
  :bug-tracker "https://github.com/ilshad/dex-edn/issues"
  :source-control (:git "git@github.com:ilshad/dex-edn.git")
  :serial t
  :depends-on ("alexandria")
  :components ((:file "packages")
	       (:file "wrappers")
	       (:file "decode")
	       (:file "encode")))
