(defpackage vend
  (:use :cl)
  (:local-nicknames (#:p #:filepaths))
  (:export #:main)
  (:documentation "Simply vendor your Common Lisp project dependencies."))

(in-package :vend)

(defun asd-files (dir)
  "Yield the pathnames of all `.asd' files found in the given DIR."
  (directory (p:join dir "*.asd")))

#++
(asd-files "./")

#++
(defun sexps-from-file (path)
  "Read the sexps from a given file PATH without evaluating them."
  (let ((*read-eval* nil))
    ()))

(defun main ()
  (format t "Done.~%"))
