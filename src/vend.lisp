(defpackage vend
  (:use :cl)
  (:local-nicknames (#:p #:filepaths)
                    (#:t #:transducers))
  (:export #:main)
  (:documentation "Simply vendor your Common Lisp project dependencies."))

(in-package :vend)

(defparameter +sources+
  '(:filepaths   "https://codeberg.org/fosskers/filepaths.git"
    :str         "https://github.com/vindarel/cl-str.git"
    :transducers "https://codeberg.org/fosskers/cl-transducers.git"))

(defun asd-files (dir)
  "Yield the pathnames of all `.asd' files found in the given DIR."
  (directory (p:join dir "*.asd")))

#++
(asd-files "./")

(defun sexps-from-file (path)
  "Read the sexps from a given file PATH without evaluating them."
  (let ((*read-eval* nil))
    (with-open-file (stream path :direction :input)
      ;; TODO: 2025-01-04 Provide similar functionality via `transducers'.
      (loop for sexp = (read stream nil :eof)
            until (eq sexp :eof)
            collect sexp))))

#++
(sexps-from-file (car (asd-files "./")))

(defun string->keyword (s)
  (intern (string-upcase s) "KEYWORD"))

(defun depends-from-sexp (sexp)
  "Extract the `:depends-on' list from a sexp, if it has one."
  (when (eq 'defsystem (car sexp))
    (t:transduce (t:map (lambda (dep)
                          (etypecase dep
                            (keyword dep)
                            (string (string->keyword dep)))))
                 #'t:snoc
                 (getf sexp :depends-on))))

#++
(depends-from-sexp (car (sexps-from-file (car (asd-files "./")))))

(defun main ()
  (format t "Done.~%"))
