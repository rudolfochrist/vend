(require :asdf)

;; Force ASDF to only look here for systems.
(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))

(format t "--- LOADING SYSTEM ---~%")
(declaim (optimize (speed 3) (debug 1) (safety 1)))
(asdf:load-system :vend)

#-ecl
(progn
  (format t "--- NO OP! ---~%")
  (format t "Use ECL instead.~%")
  #+sbcl (sb-ext:exit :code 1))

#+ecl
(progn
  (format t "--- COMPILING EXECUTABLE ---~%")
  (asdf:make-build :vend
                   :type :program
                   :move-here #p"./"
                   :epilogue-code '(vend:main))
  (format t "--- DONE ---~%")
  (ext:quit 0))
