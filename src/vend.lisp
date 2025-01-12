;;; The running of the program itself.

(in-package :vend)

;; --- Graph --- ;;

(defun scan-systems (graph dir &key (shallow nil))
  "Recursively scan directories for systems within `.asd' files and populate a
dependency graph."
  (t:transduce (t:comp (t:map #'sexps-from-file)
                       #'t:concatenate
                       (t:filter #'system?)
                       (t:map (lambda (sys)
                                (let ((name (system-name sys)))
                                  (g:add-node! graph name)
                                  (dolist (dep (depends-from-system sys))
                                    (g:add-node! graph dep)
                                    (g:add-edge! graph name dep))
                                  name))))
               #'t:cons (asd-files dir :shallow shallow)))

(defun vend/graph (&key focus)
  "Produce a dependency graph of all systems found in the current directory. If
FOCUS is supplied, only considers the subgraph with that FOCUS at the root."
  (let* ((graph (g:make-graph)))
    (scan-systems graph (ext:getcwd))
    (let ((final (cond (focus (g:subgraph graph (into-keyword focus)))
                       (t graph))))
      (with-open-file (stream #p"deps.dot" :direction :output :if-exists :supersede)
        (g:to-dot-with-stream final stream)))))

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
                                    (t:filter (lambda (leaf) (not (gethash leaf cloned))))
                                    (t:filter (lambda (leaf) (not (member leaf +exclude+)))))
                            #'t:cons (g:leaves g)))
             (recurse (top dep)
               (unless (gethash dep cloned)
                 (let ((url  (getf +sources+ dep))
                       (path (p:ensure-string (p:join target (keyword->string dep)))))
                   (assert url nil "~a is not a known system.~%Please have it registered into the vend source code." dep)
                   (format t "[vend] Cloning ~a~%" dep)
                   (clone url path)
                   (setf (gethash dep cloned) t)
                   (scan-systems graph path)
                   (dolist (leaf (unique-leaves (apply #'g:subgraph graph top)))
                     (recurse top leaf))))))
      (let* ((top  (scan-systems graph cwd))
             (root (or (get-parent (car top)) (car top))))
        ;; This is the root project directory, so it's already considered "cloned".
        (setf (gethash root cloned) t)
        (dolist (leaf (unique-leaves graph))
          (recurse top leaf))
        ;; Clean the graph one final time so that the user doesn't need to see
        ;; ugly systems that weren't asked for.
        (apply #'g:subgraph graph top)))))

#++
(let* ((cwd #p"/home/colin/code/common-lisp/kandria/")
       (dir (p:ensure-directory (p:join cwd "vendored"))))
  (with-open-file (stream #p"deps.dot" :direction :output :if-exists :supersede)
    (g:to-dot-with-stream (work cwd dir) stream)))

;; --- Executable --- ;;

(defconstant +help+
  "vend - Vendor your Common Lisp dependencies

Commands:
  get   - Download all project dependencies into 'vendored/'
  graph - Visualise a graph of all transitive project dependencies
  repl  - Start a Lisp session with only your vendored ASDF systems

Flags:
  --help    - Display this help message
  --version - Display the current version of vend
")

(defconstant +vend-rules+
  '((("--help" "-h") 0 (vend/help))
    ("--version" 0 (format t "0.1.0~%"))
    ("get"   0 (vend/get))
    ("graph" 1 (vend/graph :focus (cadr 1)) :stop)
    ("repl"  1 (vend/repl (rest 1)) :stop)))

(defun vend/help ()
  (princ +help+))

(defun vend/get ()
  "Download all dependencies."
  (let* ((cwd (ext:getcwd))
         (dir (p:ensure-directory (p:join cwd "vendored"))))
    (cond ((probe-file dir)
           (format t "[vend] Target directory already exists.~%")
           (ext:quit 1))
          (t (format t "[vend] Downloading dependencies.~%")
             (work cwd dir)
             (format t "[vend] Done.~%")))))

(defun vend/repl (args)
  "Start a given repl."
  (let ((compiler (or (car args) "sbcl"))
        (load '("--eval" "(require :asdf)" "--eval" "(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))")))
    (ext:run-program compiler (append (cdr args) load) :output t :input *standard-input*)))

(defun main ()
  (let ((ext:*lisp-init-file-list* nil)
        (ext:*help-message* +help+))
    (cond ((= 1 (length ext:*command-args*)) (vend/help))
          (t (ext:process-command-args :rules +vend-rules+)))
    (ext:quit 0)))

;; Vendor lem
;; vend get --rmgit
;; vend open <foo>  <- opens project URL in browser
;; vend graph <pkg> <- subgraph of this guy
;; vend check

;; Success:
;;
;; Trial
;; Alloy
;; Kandria
;; Nyxt
;; Lem

;; Failure:
;;
;; qlot <- impossible due to `package-inferred-system'.
;; cl-torrents <- mockingbird using `package-inferred-system'.

;; Bad boys:
;; https://github.com/slyrus/opticl/blob/master/opticl-doc.asd
;; https://github.com/rpav/fast-io/blob/master/fast-io-test.asd
;; https://github.com/sharplispers/ironclad/blob/master/ironclad.asd
;; https://github.com/sharplispers/chipz/blob/master/chipz.asd
;; https://github.com/rpav/fast-io/blob/master/fast-io.asd#L15
