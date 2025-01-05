(defpackage vend
  (:use :cl)
  (:local-nicknames (#:p #:filepaths)
                    (#:t #:transducers))
  (:export #:main)
  (:documentation "Simply vendor your Common Lisp project dependencies."))

(in-package :vend)

(defparameter +parents+
  '(:cffi-grovel :cffi
    :cffi-toolchain :cffi
    :cl-ppcre-unicode :cl-ppcre
    :dref :mgl-pax
    :mgl-pax-bootstrap :mgl-pax
    :regression-test :ansi-test
    :rt :ansi-test
    :trivia.balland2006 :trivia
    :uiop :asdf)
  "Systems are often bundled together into a single repository. This list helps
map back to the parent, such that later only one git clone is performed.")

(defparameter +exclude+
  '(;; Not hosted on any public forges.
    :cl-postgres
    :hu.dwim.presentation
    :hu.dwim.web-server
    :puri)
  "Known naughty systems that we can't do anything about.")

(defparameter +sources+
  '(:alexandria      "https://gitlab.common-lisp.net/alexandria/alexandria.git"
    :anaphora        "https://github.com/spwhitton/anaphora.git"
    :ansi-test       "https://gitlab.common-lisp.net/ansi-test/ansi-test.git"
    :asdf            "https://gitlab.common-lisp.net/asdf/asdf.git"
    :babel           "https://github.com/cl-babel/babel.git"
    :bordeaux-threads "https://github.com/sionescu/bordeaux-threads.git"
    :cffi            "https://github.com/cffi/cffi.git"
    :cl-change-case  "https://github.com/rudolfochrist/cl-change-case.git"
    :cl-fad          "https://github.com/edicl/cl-fad.git"
    :cl-json         "https://github.com/sharplispers/cl-json.git"
    :cl-l10n         "https://gitlab.common-lisp.net/cl-l10n/cl-l10n.git"
    :cl-ppcre        "https://github.com/edicl/cl-ppcre.git"
    :cl-unicode      "https://github.com/edicl/cl-unicode.git"
    :closer-mop      "https://github.com/pcostanza/closer-mop.git"
    :closure-common  "https://github.com/sharplispers/closure-common.git"
    :contextl        "https://github.com/pcostanza/contextl.git"
    :command-line-arguments "https://github.com/fare/command-line-arguments.git"
    :cxml            "https://github.com/sharplispers/cxml.git"
    :documentation-utils "https://github.com/Shinmera/documentation-utils.git"
    :fare-quasiquote "https://gitlab.common-lisp.net/frideau/fare-quasiquote.git"
    :fare-utils      "https://gitlab.common-lisp.net/frideau/fare-utils.git"
    :form-fiddle     "https://github.com/Shinmera/form-fiddle.git"
    :fiasco          "https://github.com/joaotavora/fiasco.git"
    :filepaths       "https://codeberg.org/fosskers/filepaths.git"
    :fiveam          "https://github.com/lispci/fiveam.git"
    :flexi-streams   "https://github.com/edicl/flexi-streams.git"
    :fset            "https://gitlab.common-lisp.net/fset/fset.git"
    :hu.dwim.common  "https://github.com/hu-dwim/hu.dwim.common.git"
    :hu.dwim.common-lisp "https://github.com/hu-dwim/hu.dwim.common-lisp.git"
    :hu.dwim.def     "https://github.com/hu-dwim/hu.dwim.def.git"
    :hu.dwim.defclass-star "https://github.com/hu-dwim/hu.dwim.defclass-star.git"
    :hu.dwim.delico  "https://github.com/hu-dwim/hu.dwim.delico.git"
    :hu.dwim.logger  "https://github.com/hu-dwim/hu.dwim.logger.git"
    :hu.dwim.partial-eval "https://github.com/hu-dwim/hu.dwim.partial-eval.git"
    :hu.dwim.stefil  "https://github.com/hu-dwim/hu.dwim.stefil.git"
    :hu.dwim.syntax-sugar "https://github.com/hu-dwim/hu.dwim.syntax-sugar.git"
    :hu.dwim.util    "https://github.com/hu-dwim/hu.dwim.util.git"
    :hu.dwim.walker  "https://github.com/hu-dwim/hu.dwim.walker.git"
    :introspect-environment "https://github.com/Bike/introspect-environment.git"
    :iolib           "https://github.com/sionescu/iolib.git"
    :iterate         "https://gitlab.common-lisp.net/iterate/iterate.git"
    :lift            "https://github.com/hraban/lift.git"
    :lisp-namespace  "https://github.com/guicho271828/lisp-namespace.git"
    :local-time      "https://github.com/dlowe-net/local-time.git"
    :metabang-bind   "https://github.com/hraban/metabang-bind.git"
    :mgl-pax         "https://github.com/melisgl/mgl-pax.git"
    :misc-extensions "https://gitlab.common-lisp.net/misc-extensions/misc-extensions.git"
    :named-readtables "https://github.com/melisgl/named-readtables.git"
    :optima          "https://github.com/m2ym/optima.git"
    :parachute       "https://github.com/Shinmera/parachute.git"
    :parse-number    "https://github.com/sharplispers/parse-number.git"
    :pythonic-string-reader "https://github.com/smithzvk/pythonic-string-reader.git"
    :random-state    "https://github.com/Shinmera/random-state.git"
    :split-sequence  "https://github.com/sharplispers/split-sequence.git"
    :str             "https://github.com/vindarel/cl-str.git"
    :swank           "https://github.com/slime/slime.git"
    :transducers     "https://codeberg.org/fosskers/cl-transducers.git"
    :trivia          "https://github.com/guicho271828/trivia.git"
    :trivial-cltl2   "https://github.com/Zulu-Inuoe/trivial-cltl2.git"
    :trivial-custom-debugger "https://github.com/phoe/trivial-custom-debugger.git"
    :trivial-garbage "https://github.com/trivial-garbage/trivial-garbage.git"
    :trivial-gray-streams "https://github.com/trivial-gray-streams/trivial-gray-streams.git"
    :trivial-features "https://github.com/trivial-features/trivial-features.git"
    :trivial-indent  "https://github.com/Shinmera/trivial-indent.git"
    :try             "https://github.com/melisgl/try.git"
    :type-i          "https://github.com/guicho271828/type-i.git"
    :com.inuoe.jzon  "https://github.com/Zulu-Inuoe/jzon.git")
  "All actively depended-on Common Lisp libraries.")

(defun asd-files (dir)
  "Yield the pathnames of all `.asd' files found in the given DIR."
  (directory (p:join dir "*.asd")))

#++
(asd-files "./")

(defun string-from-file (path)
  "Preserves newlines, such that comments will be handled properly by `read'."
  (t:transduce (t:comp (t:map (lambda (line) (string-trim " " line)))
                       (t:intersperse '(#\Newline))
                       #'t:concatenate)
               #'t:string path))

#++
(string-from-file #p"vend.asd")

(defun sexps-from-file (path)
  "Read the sexps from a given file PATH without evaluating them."
  (format t "[vend] Extracting systems from ~a~%" path)
  (let* ((str    (string-from-file path))
         (clean  (remove-reader-chars str))
         (stream (make-string-input-stream clean)))
    ;; TODO: 2025-01-04 Provide similar functionality via `transducers'.
    (loop for sexp = (read stream nil :eof)
          until (eq sexp :eof)
          collect sexp)))

#++
(sexps-from-file (car (asd-files "./")))

(defun reader-macro? (chars)
  (and (eql #\# (nth 0 chars))
       (eql #\. (nth 1 chars))
       (eql #\( (nth 2 chars))))

#++
(reader-macro? (coerce "#.(+ 1 1)" 'list))
#++
(reader-macro? (coerce "(+ 1 1)" 'list))
#++
(reader-macro? (coerce "" 'list))

(defun asdf-call? (chars)
  (and (eql #\( (nth 0 chars))
       (eql #\a (nth 1 chars))
       (eql #\s (nth 2 chars))
       (eql #\d (nth 3 chars))
       (eql #\f (nth 4 chars))
       (eql #\: (nth 5 chars))))

#++
(asdf-call? (coerce "(asdf:foo)" 'list))
#++
(asdf-call? (coerce "(foo)" 'list))

(defun uiop-call? (chars)
  (and (eql #\( (nth 0 chars))
       (eql #\u (nth 1 chars))
       (eql #\i (nth 2 chars))
       (eql #\o (nth 3 chars))
       (eql #\p (nth 4 chars))
       (eql #\: (nth 5 chars))))

(defun remove-reader-chars (str)
  "Replace any `#.' sexp with T."
  (labels ((keep (acc chars)
             (let ((head (car chars))
                   (tail (cdr chars)))
               (cond ((null head) acc)
                     ;; The presence of read-time macros confuses `read', so we
                     ;; proactively remove them.
                     ((reader-macro? chars)
                      (keep (cons #\t acc) (chuck 1 (cddr tail))))
                     ;; Likewise, some clever package authors like to utilise
                     ;; `asdf' and `uiop' directly in their system definitions.
                     ;; This similarly causes problems with `read', so we remove
                     ;; such calls.
                     ((or (asdf-call? chars)
                          (uiop-call? chars))
                      (keep (cons #\( acc) (nthcdr 5 tail)))
                     (t (keep (cons head acc) tail)))))
           (chuck (parens chars)
             (let ((head (car chars)))
               (cond ((zerop parens) chars)
                     ((null head) '())
                     ((eql #\( head) (chuck (1+ parens) (cdr chars)))
                     ((eql #\) head) (chuck (1- parens) (cdr chars)))
                     (t (chuck parens (cdr chars)))))))
    (coerce (reverse (keep '() (coerce str 'list)))
            'string)))

#++
(remove-reader-chars "(asdf:defsystem :foo :long-description #.(+ 1 1) :foo (asdf:bar))")

(defun string->keyword (s)
  (intern (string-upcase s) "KEYWORD"))

(defun symbol->keyword (s)
  (intern (symbol-name s) "KEYWORD"))

#++
(symbol->keyword 'foo)

(defun keyword->string (kw)
  (t:transduce (t:map (lambda (c) (if (equal #\. c) #\-  c)))
               #'t:string (string-downcase (format nil "~a" kw))))

#++
(keyword->string :KW)
#++
(keyword->string :com.inuoe.jzon)

(defun system? (sexp)
  (and (eq 'cons (type-of sexp))
       (string= "DEFSYSTEM" (symbol-name (car sexp)))))

(defun depends-from-system (sexp)
  "Extract the `:depends-on' list from a sexp, if it has one."
  (t:transduce (t:map (lambda (dep)
                        (etypecase dep
                          (keyword dep)
                          (string (string->keyword dep))
                          (symbol (symbol->keyword dep))
                          (list (destructuring-bind (kw name v) dep
                                  (declare (ignore v))
                                  (cond ((and (eq :version kw) (stringp name))
                                         (string->keyword name))
                                        (t (error "Unknown composite dependency declaration: ~a" dep))))))))
               #'t:snoc
               (getf sexp :depends-on)))

#++
(depends-from-system (car (sexps-from-file (car (asd-files "./")))))

(defun system-name (sexp)
  (let ((name (nth 1 sexp)))
    (etypecase name
      (keyword name)
      (string (string->keyword name))
      (symbol (string->keyword (symbol-name name))))))

#++
(system-name (car (sexps-from-file (car (asd-files "./")))))
#++
(system-name '(defsystem foo))

(defun mkdir (dir)
  (multiple-value-bind (stream code obj)
      (ext:run-program "mkdir" (list "-p" (p:ensure-string dir)))
    (declare (ignore stream obj))
    (assert (= 0 code))))

(defun clone (url path)
  "Given a source URL to clone from, do a shallow git clone into a given absolute PATH."
  (multiple-value-bind (stream code obj)
      (ext:run-program "git" (list "clone" "--depth=1" url path) :output t)
    (declare (ignore stream obj))
    (assert (= 0 code) nil "Clone failed: ~a" url)))

(defun work (cwd target)
  "Recursively perform a git clone on every detected dependency."
  (let ((cache (make-hash-table)))
    (labels ((recurse (dep-dir)
               (format t "[vend] Scanning ~a~%" dep-dir)
               (t:transduce
                (t:comp (t:map #'sexps-from-file)
                        #'t:concatenate
                        (t:filter #'system?)
                        ;; If we're at the top level, we need to preemptively
                        ;; add its systems to the cache. This prevents
                        ;; reclonings of the project itself in certain
                        ;; circumstances.
                        (t:map (lambda (sys)
                                 (when (equal dep-dir cwd)
                                   (setf (gethash (system-name sys) cache) t))
                                 sys))
                        ;; Here we ensure that only systems that were asked for
                        ;; at higher levels are actually scanned for
                        ;; dependencies.
                        (t:filter (lambda (sys) (gethash (system-name sys) cache)))
                        (t:log (lambda (acc sys)
                                 (declare (ignore acc))
                                 (format t "[vend] Analysing system: ~a~%" (system-name sys))))
                        (t:map #'depends-from-system)
                        #'t:concatenate
                        #'t:unique
                        (t:map (lambda (dep) (or (getf +parents+ dep) dep)))
                        #'t:unique
                        (t:filter (lambda (dep) (not (gethash dep cache))))
                        (t:filter (lambda (dep) (not (member dep +exclude+))))
                        (t:map (lambda (dep)
                                 (let ((source (getf +sources+ dep))
                                       (cloned (p:ensure-string (p:join target (keyword->string dep)))))
                                   (assert source nil "~a is not a known system.~%Please have it registered into the vend source code." dep)
                                   (setf (gethash dep cache) t)
                                   (clone source cloned)
                                   (recurse cloned)))))
                #'t:for-each
                (asd-files dep-dir))))
      (mkdir target)
      (recurse cwd))))

#++
(let* ((cwd (ext:getcwd))
       (dir (p:ensure-directory (p:join cwd "vendored2"))))
  (work cwd dir))

#++
(let* ((cwd #p"/home/colin/code/common-lisp/transducers/")
       (dir (p:ensure-directory (p:join cwd "vendored"))))
  (work cwd dir))

;; TODO: 2025-01-05 Expose flag to avoid cloning `asdf'.
(defun main ()
  (let* ((cwd (ext:getcwd))
         (dir (p:ensure-directory (p:join cwd "vendored"))))
    (cond ((probe-file dir)
           (format t "[vend] Target directory already exists.~%")
           (si:exit 1))
          (t (work cwd dir)
             (format t "[vend] Done.~%")))))
