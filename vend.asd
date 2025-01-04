(defsystem "vend"
  :version "0.1.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :homepage "https://github.com/fosskers/vend"
  :depends-on ()
  :components ((:module "src" :components ((:file "vend"))))
  :description "Simply vendor your Common Lisp project dependencies.")
