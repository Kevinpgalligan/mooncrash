(defpackage :mooncrash-asd
  (:use :cl :asdf))

(in-package :mooncrash-asd)

(defsystem mooncrash
  :license "MIT"
  :author "Kevin Galligan"
  :depends-on (:trivial-gamekit)
  :pathname "src"
  :serial t
  :components ((:file "package")
	       (:file "vec")
               (:file "game")))

