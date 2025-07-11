;;; The running of the program itself.

(in-package :vend)

;; --- Constants --- ;;

(defparameter +require-asdf+ "(require \"asdf\")")
(defparameter +init-registry+ "(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))")

;; --- Graph --- ;;

(defun scan-systems! (graph paths)
  "Given a collection of paths to asd files, extract all their system definitions
and mutably add them to a given graph. As a return value, yields the keyword
names of the added systems."
  (t:transduce (t:comp #++(t:log (lambda (acc path) (vlog "Reading ~a" path)))
                       (t:map #'systems-from-file)
                       #'t:concatenate
                       (t:map (lambda (sys)
                                (let ((name (system-name sys)))
                                  (g:add-node! graph name)
                                  (dolist (dep (depends-from-system sys))
                                    (g:add-node! graph dep)
                                    (g:add-edge! graph name dep))
                                  name))))
               #'t:cons paths))

(defun vend/graph (&key focus)
  "Produce a dependency graph of all systems depended upon by systems of the root
project. If FOCUS is supplied, only considers the subgraph with that FOCUS as
the root."
  (let* ((graph (g:make-graph))
         (top   (scan-systems! graph (root-asd-files (ext:getcwd)))))
    (scan-systems! graph (asd-files (p:join (ext:getcwd) "vendored")))
    (let ((final (cond (focus (g:subgraph graph (into-keyword focus)))
                       (t (apply #'g:subgraph graph top)))))
      (with-open-file (stream #p"deps.dot" :direction :output :if-exists :supersede)
        (g:to-dot-with-stream final stream)))))

;; --- Search --- ;;

(defun vend/search (term)
  "Check the names of all known systems for a given term."
  (let* ((matches (t:transduce (t:comp (t:map (lambda (pair)
                                                (cons (keyword->string (car pair))
                                                      (cdr pair))))
                                       (t:filter (lambda (pair) (substring? (car pair) term))))
                               #'t:cons (t:plist +sources+)))
         (longest (t:transduce (t:map (lambda (pair) (length (car pair))))
                               (t:fold #'max 0) matches)))
    (dolist (pair matches)
      (format t "~va ~a~%" longest (car pair) (cdr pair)))))

#++
(vend/search "nonexistent")

;; --- Check --- ;;

(defun vend/check (&key focus)
  "Check the dependency graph for old deps, etc."
  (let* ((graph (g:make-graph))
         (top   (scan-systems! graph (root-asd-files (ext:getcwd)))))
    (scan-systems! graph (asd-files (p:join (ext:getcwd) "vendored")))
    (let ((final (cond (focus (g:subgraph graph (into-keyword focus)))
                       (t (apply #'g:subgraph graph top)))))
      (t:transduce (t:comp (t:map #'car)
                           (t:filter-map (lambda (sys)
                                           (let ((parent (get-parent sys)))
                                             (cond ((member parent +deprecated+) (cons :deprecated sys))
                                                   ((member parent +missing+) (cons :missing sys))))))
                           (t:map (lambda (pair)
                                    (let* ((sys (cdr pair))
                                           (reason (car pair))
                                           (routes  (g:paths-to final sys))
                                           (longest (reverse (cdr (t:transduce (t:map (lambda (route) (cons (length route) route)))
                                                                               (t:fold (lambda (a b) (if (> (car a) (car b)) a b)))
                                                                               routes)))))
                                      (case reason
                                        (:deprecated (format t "~a is deprecated.~%" (bold-red sys)))
                                        (:missing (format t "~a is not publically available and so was not vendored!~%" (bold-red sys))))
                                      (format t "  ~{~a~^ -> ~}~%" longest)))))
                   #'t:for-each (g:graph-nodes final)))))

#++
(vend/check)

;; --- Downloading --- ;;

(defun clone (url path name)
  "Given a source URL to clone from, do a shallow git clone into a given absolute PATH."
  (unless (probe-file path)
    (let* ((program "git")
           (project-name (string-downcase (string name)))
           (arguments (list "subtree" "add" "-q" "--prefix" (concatenate 'string "vendored/" project-name) url
                            "HEAD" "--squash" "-m" (format nil "Add dependency ~A" name))))
      (multiple-value-bind (stream code obj)
          (ext:run-program program arguments :output t)
        (declare (ignore stream obj))
        (assert (= 0 code) nil "Clone failed: ~a" url)))))

(defun work (cwd target)
  "Recursively perform a git clone on every detected dependency."
  (let ((graph  (g:make-graph))
        (cloned (make-hash-table)))
    (labels ((unique-leaves (g)
               (t:transduce (t:comp (t:map (lambda (leaf) (or (get-parent leaf) leaf)))
                                    #'t:unique
                                    (t:filter (lambda (leaf)
                                                (not (or (gethash leaf cloned)
                                                         (member leaf +exclude+)
                                                         (member leaf +missing+))))))
                            #'t:cons (g:leaves g)))
             (recurse (top dep)
               (unless (gethash dep cloned)
                 (let ((url  (getf +sources+ dep))
                       (path (p:ensure-string (p:join target (keyword->string dep)))))
                   (unless (or url (probe-file path))
                     (let ((route (reverse (car (g:paths-to graph dep)))))
                       (error "~a is not a known system.~%~%  ~{~a~^ -> ~}~%~%Please have it registered in the vend source code." (bold-red dep) route)))
                   (vlog "Fetching ~a" (bold dep))
                   (clone url path (keyword->string dep))
                   (setf (gethash dep cloned) t)
                   (scan-systems! graph (asd-files path))
                   (dolist (leaf (unique-leaves (apply #'g:subgraph graph top)))
                     (recurse top leaf))))))
      (let* ((top  (scan-systems! graph (root-asd-files cwd)))
             (root (t:transduce (t:comp (t:map #'get-parent)
                                        #'t:unique)
                                #'t:first top)))
        ;; This is the root project directory, so it's already considered "cloned".
        (setf (gethash root cloned) t)
        (dolist (leaf (unique-leaves graph))
          (recurse top leaf))))))

#++
(let* ((cwd #p"/home/colin/code/common-lisp/rtg-math/")
       (dir (p:ensure-directory (p:join cwd "vendored"))))
  (work cwd dir))

;; --- Project Initialization --- ;;

(defconstant +defsystem-template+
  "(defsystem \"~a\"
  :version \"0.0.0\"
  :author \"\"
  :license \"\"
  :homepage \"\"
  :depends-on ()
  :serial t
  :components ((:module \"src\" :components ((:file \"package\"))))
  :description \"\")
")

(defconstant +defpackage-template+
  "(defpackage ~a
  (:use :cl)
  (:documentation \"\"))

(in-package :~a)
")

(defun vend/init (name)
  "Given the name of a project, create a simple directory structure with a minimal .asd file."
  (ensure-directories-exist (p:ensure-directory (p:join name "src")))
  (with-open-file (f (p:join name (p:with-extension name "asd"))
                     :direction :output
                     :if-does-not-exist :create)
    (format f +defsystem-template+ name))
  (with-open-file (f (p:join name "src" "package.lisp")
                     :direction :output
                     :if-does-not-exist :create)
    (format f +defpackage-template+ name name)))

;; --- Executable --- ;;

(defconstant +help+
  "vend - Vendor your Common Lisp dependencies

Commands:
  check  [focus] - Check your dependencies for issues
  get            - Download all project dependencies into 'vendored/'
  graph  [focus] - Visualise a graph of transitive project dependencies
  init   [name]  - Create a minimal project skeleton
  repl   [args]  - Start a Lisp session with only your vendored ASDF systems
  search [term]  - Search known systems
  test   [args]  - Run all detected test systems

Flags:
  --help    - Display this help message
  --version - Display the current version of vend
")

(defconstant +vend-rules+
  '((("--help" "-h") 0 (vend/help))
    ("--version" 0 (format t "0.2.0~%"))
    ("check"  1 (vend/check :focus (cadr 1)) :stop)
    ("get"    0 (vend/get))
    ("graph"  1 (vend/graph :focus (cadr 1)) :stop)
    ("init"   1 (vend/init (cadr 1)) :stop)
    ("repl"   1 (vend/repl (rest 1)) :stop)
    ("search" 1 (vend/search 1))
    ("test"   1 (vend/test (rest 1)) :stop)))

(defun vend/help ()
  (princ +help+))

(defun vend/get ()
  "Download all dependencies."
  (let* ((cwd (ext:getcwd))
         (dir (p:ensure-directory (p:join cwd "vendored"))))
    (vlog "Downloading dependencies.")
    (handler-bind ((error (lambda (c)
                            (format t "~a~%" c)
                            (ext:quit 1))))
      (work cwd dir))
    (vlog "Done.")))

(defun vend/test (args &key (dir (ext:getcwd)))
  "Run detected test systems."
  (let* ((compiler (or (car args) "sbcl"))
         (eval (eval-flag compiler))
         (clisp (if (clisp? compiler) '("-repl") '()))
         (systems (t:transduce (t:comp (t:map #'systems-from-file)
                                       #'t:concatenate)
                               #'t:cons (root-asd-files dir)))
         (tests (test-invocations systems)))
    (when tests
      (let ((exps (t:transduce (t:comp (t:intersperse eval)
                                       (t:once eval))
                               #'t:cons (append (list +require-asdf+ +init-registry+) tests))))
        (vlog "Running tests.")
        (multiple-value-bind (stream code state)
            (ext:run-program compiler (append (cdr args) clisp exps) :output *standard-output*)
          (declare (ignore stream state))
          (unless (zerop code)
            (ext:quit 1)))))))

#++
(vend/test '() :dir #p"/home/colin/code/common-lisp/filepaths/")

(defun vend/repl (args)
  "Start a given repl."
  (let* ((compiler (or (car args) "sbcl"))
         (eval (eval-flag compiler))
         (load (list eval +require-asdf+ eval +init-registry+))
         (clisp (if (clisp? compiler) '("-repl") '())))
    (ext:run-program compiler (append (cdr args) clisp load) :output t :input *standard-input*)))

(defun main ()
  (let ((ext:*lisp-init-file-list* nil)
        (ext:*help-message* +help+))
    (cond ((= 1 (length ext:*command-args*)) (vend/help))
          (t (ext:process-command-args :rules +vend-rules+)))
    (ext:quit 0)))

;; Bad boys:
;; https://github.com/slyrus/opticl/blob/master/opticl-doc.asd
;; https://github.com/rpav/fast-io/blob/master/fast-io-test.asd
;; https://github.com/sharplispers/ironclad/blob/master/ironclad.asd
;; https://github.com/sharplispers/chipz/blob/master/chipz.asd
;; https://github.com/rpav/fast-io/blob/master/fast-io.asd#L15

#++
(t:transduce (t:comp (t:once 2) (t:once 1)) #'t:cons '(3 4 5))
