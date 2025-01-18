;;; The running of the program itself.

(in-package :vend)

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

(defun clone (url path)
  "Given a source URL to clone from, do a shallow git clone into a given absolute PATH."
  (unless (probe-file path)
    (multiple-value-bind (stream code obj)
        (ext:run-program "git" (list "clone" "--quiet" "--depth=1" url path) :output t)
      (declare (ignore stream obj))
      (assert (= 0 code) nil "Clone failed: ~a" url))))

(defun work (cwd target)
  "Recursively perform a git clone on every detected dependency."
  (let ((graph  (g:make-graph))
        (cloned (make-hash-table)))
    (labels ((unique-leaves (g)
               (t:transduce (t:comp (t:map (lambda (leaf) (or (get-parent leaf) leaf)))
                                    #'t:unique
                                    (t:filter (lambda (leaf) (not (or (gethash leaf cloned)
                                                                      (member leaf +exclude+)
                                                                      (member leaf +missing+))))))
                            #'t:cons (g:leaves g)))
             (recurse (top dep)
               (unless (gethash dep cloned)
                 (let ((url  (getf +sources+ dep))
                       (path (p:ensure-string (p:join target (keyword->string dep)))))
                   (unless url
                     (let ((route (reverse (car (g:paths-to graph dep)))))
                       (error "~a is not a known system.~%~%  ~{~a~^ -> ~}~%~%Please have it registered in the vend source code" (bold-red dep) route)))
                   (vlog "Fetching ~a" (bold dep))
                   (clone url path)
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
(let* ((cwd #p"/home/colin/code/common-lisp/cl-gtk4/")
       (dir (p:ensure-directory (p:join cwd "vendored"))))
  (work cwd dir))

;; TODO: Implement `vend get foo' that tricks it into downloading the dep graph
;; of that specific dep, even if it isn't listed in a local project's ASD file.

;; --- Executable --- ;;

(defconstant +help+
  "vend - Vendor your Common Lisp dependencies

Commands:
  check [focus] - Check your dependencies for issues
  get           - Download all project dependencies into 'vendored/'
  graph [focus] - Visualise a graph of transitive project dependencies
  repl  [args]  - Start a Lisp session with only your vendored ASDF systems

Flags:
  --help    - Display this help message
  --version - Display the current version of vend
")

(defconstant +vend-rules+
  '((("--help" "-h") 0 (vend/help))
    ("--version" 0 (format t "0.1.2~%"))
    ("check" 1 (vend/check :focus (cadr 1)) :stop)
    ("get"   0 (vend/get))
    ("graph" 1 (vend/graph :focus (cadr 1)) :stop)
    ("repl"  1 (vend/repl (rest 1)) :stop)))

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

(defun vend/repl (args)
  "Start a given repl."
  (let* ((compiler (or (car args) "sbcl"))
         (eval (if (string-equal "alisp" compiler) "-e" "--eval"))
         (load (list eval "(require :asdf)" eval "(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))")))
    (ext:run-program compiler (append (cdr args) load) :output t :input *standard-input*)))

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
