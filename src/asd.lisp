;;; The parsing of ASD files and extraction of System data.

(in-package :vend)

(defun components-less? (a b)
  "Comparison of path components."
  (labels ((recurse (ac bc)
             (cond ((null ac) t)
                   ((and (not (null ac)) (null bc)) nil)
                   ((and (null (cdr ac)) (not (null (cdr bc)))) t)
                   ((and (not (null (cdr ac))) (null (cdr bc))) nil)
                   ((and (null (cdr ac)) (null (cdr bc)))
                    (string-lessp (p:base (car ac))
                                  (p:base (car bc))))
                   ((string-lessp (car ac) (car bc)) t)
                   ((string-lessp (car bc) (car ac)) nil)
                   (t (recurse (cdr ac) (cdr bc))))))
    (recurse (p:components a) (p:components b))))

#++
(components-less? "trivial-gray-streams.asd" "trivial-gray-streams-test.asd")
#++
(components-less? #p"/home/colin/foo" #p"/home/colin/foo/bar")
#++
(components-less? #p"/home/colin/foo/bar" #p"/home/colin/foo")

(defun asd-files (dir &key (shallow nil))
  "Yield the pathnames of all `.asd' files found in the given DIR."
  (let ((patt (if shallow "*.asd" "**/*.asd")))
    (sort (directory (p:join dir patt))
          #'components-less?)))

#++
(asd-files "./")
#++
(asd-files "/home/colin/code/common-lisp/trial/vendored/com-inuoe-jzon/")

#++
(asd-files "/home/colin/code/common-lisp/transducers/vendored/parachute/")

#++
(asd-files "/home/colin/code/common-lisp/transducers/vendored/trivial-gray-streams/")

#++
(p:components "/home/colin/code/common-lisp/transducers/vendored/parachute/")

(defun comment? (string)
  "Is the given STRING a comment line?"
  (unless (zerop (length string))
    (eql #\; (aref string 0))))

#++
(comment? "")
#++
(comment? "; Hello!")

(defun chipz? (string)
  "A hack to account for `chipz.asd'."
  (or (string-starts-with? string "#+chipz-system:")
      (string-starts-with? string "#-chipz-system:")))

#++
(chipz? "#+chipz-system:gray-streams")

(defun string-from-file (path)
  "Preserves newlines but removes whole-line comments."
  (t:transduce (t:comp (t:map (lambda (line) (string-trim " " line)))
                       (t:filter (lambda (line) (not (comment? line))))
                       ;; HACK: 2025-01-11 chipz is a naughty system.
                       (t:filter (lambda (line) (not (chipz? line))))
                       (t:intersperse '(#\Newline))
                       #'t:concatenate)
               #'t:string path))

#++
(string-from-file #p"vend.asd")

(defun sexps-from-file (path)
  "Read the sexps from a given file PATH without evaluating them."
  (let* ((str    (string-from-file path))
         (clean  (remove-reader-chars str))
         (stream (make-string-input-stream clean)))
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

(defun quoted-asdf-call? (chars)
  (and (eql #\' (nth 0 chars))
       (eql #\a (nth 1 chars))
       (eql #\s (nth 2 chars))
       (eql #\d (nth 3 chars))
       (eql #\f (nth 4 chars))
       (eql #\: (nth 5 chars))))

(defun asdf-/-call? (chars)
  (and (eql #\( (nth 0 chars))
       (eql #\a (nth 1 chars))
       (eql #\s (nth 2 chars))
       (eql #\d (nth 3 chars))
       (eql #\f (nth 4 chars))
       (eql #\/ (nth 5 chars))))

(defun uiop-call? (chars)
  (and (eql #\( (nth 0 chars))
       (eql #\u (nth 1 chars))
       (eql #\i (nth 2 chars))
       (eql #\o (nth 3 chars))
       (eql #\p (nth 4 chars))
       (eql #\: (nth 5 chars))))

(defun grovel-call? (chars)
  (and (eql #\( (nth 0 chars))
       (eql #\c (nth 1 chars))
       (eql #\f (nth 2 chars))
       (eql #\f (nth 3 chars))
       (eql #\i (nth 4 chars))
       (eql #\- (nth 5 chars))
       (eql #\g (nth 6 chars))
       (eql #\r (nth 7 chars))
       (eql #\o (nth 8 chars))
       (eql #\v (nth 9 chars))
       (eql #\e (nth 10 chars))
       (eql #\l (nth 11 chars))
       (eql #\: (nth 12 chars))))

(defun def? (chars)
  (and (eql #\( (nth 0 chars))
       (eql #\d (nth 1 chars))
       (eql #\e (nth 2 chars))
       (eql #\f (nth 3 chars))
       (or (eql #\c (nth 4 chars))
           (eql #\m (nth 4 chars)))))

(defun checkl? (chars)
  (and (eql #\( (nth 0 chars))
       (eql #\c (nth 1 chars))
       (eql #\h (nth 2 chars))
       (eql #\e (nth 3 chars))
       (eql #\c (nth 4 chars))
       (eql #\k (nth 5 chars))
       (eql #\l (nth 6 chars))
       (eql #\: (nth 7 chars))))

(defun eval-when? (chars)
  (and (eql #\( (nth 0 chars))
       (eql #\e (nth 1 chars))
       (eql #\v (nth 2 chars))
       (eql #\a (nth 3 chars))
       (eql #\l (nth 4 chars))
       (eql #\- (nth 5 chars))
       (eql #\w (nth 6 chars))
       (eql #\h (nth 7 chars))
       (eql #\e (nth 8 chars))
       (eql #\n (nth 9 chars))))

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
                     ((quoted-asdf-call? chars)
                      (keep (cons #\' acc) (nthcdr 5 tail)))
                     ((grovel-call? chars)
                      (keep (cons #\( acc) (nthcdr 12 tail)))
                     ((def? chars)
                      (keep (cons #\t acc) (chuck 1 (cddr tail))))
                     ((checkl? chars)
                      (keep (cons #\( acc) (nthcdr 7 tail)))
                     ((eval-when? chars)
                      (keep (cons #\t acc) (chuck 1 (cddr tail))))
                     ((asdf-/-call? chars)
                      (keep (cons #\( acc) (until-colon tail)))
                     (t (keep (cons head acc) tail)))))
           (chuck (parens chars)
             (let ((head (car chars)))
               (cond ((zerop parens) chars)
                     ((null head) '())
                     ((eql #\( head) (chuck (1+ parens) (cdr chars)))
                     ((eql #\) head) (chuck (1- parens) (cdr chars)))
                     (t (chuck parens (cdr chars))))))
           (until-colon (chars)
             (let ((head (car chars)))
               (cond ((null head) '())
                     ((eql #\: head) (cdr chars))
                     (t (until-colon (cdr chars)))))))
    (coerce (reverse (keep '() (coerce str 'list)))
            'string)))

#++
(remove-reader-chars "(defsystem foo)")
#++
(remove-reader-chars "(asdf:defsystem :foo :long-description #.(+ 1 1) :foo (asdf:bar))")
#++
(remove-reader-chars "(cffi-grovel:foo 1)")
#++
(remove-reader-chars "(checkl:foo 1)")
#++
(remove-reader-chars "(defmethod foo (a b) (+ 1 1)) (hi)")
#++
(remove-reader-chars (string-from-file #p"/home/colin/code/common-lisp/trial/vendored/trivial-features/trivial-features-tests.asd"))
#++
(remove-reader-chars "(asdf/parse-defsystem:defsystem foo)")

(defun system? (sexp)
  (and (eq 'cons (type-of sexp))
       (string= "DEFSYSTEM" (symbol-name (car sexp)))))

(defun depends-from-system (sexp)
  "Extract the `:depends-on' list from a sexp, if it has one."
  (t:transduce (t:filter-map
                (lambda (dep)
                  (etypecase dep
                    (keyword dep)
                    (string (into-keyword dep))
                    (symbol (into-keyword dep))
                    (list (destructuring-bind (kw a &optional b) dep
                            (cond ((eq :version kw) (into-keyword a))
                                  ((eq :require kw) (into-keyword a))
                                  ;; To account for Radiance's higher-level
                                  ;; implementation injection mechanism.
                                  ;; `:interface' is otherwise not a keyword
                                  ;; supported by ASDF.
                                  ((eq :interface kw) nil)
                                  ((eq :feature kw)
                                   (typecase b
                                     (list (into-keyword (cadr b)))
                                     (t    (into-keyword b))))
                                  (t (error "Unknown composite dependency declaration: ~a" dep))))))))
               #'t:snoc
               (getf sexp :depends-on)))

#++
(depends-from-system (car (sexps-from-file (car (asd-files "/home/colin/code/common-lisp/trial/vendored/com-inuoe-jzon/")))))

(defun system-name (sexp)
  (let ((name (nth 1 sexp)))
    (etypecase name
      (keyword name)
      (string (into-keyword name))
      (symbol (into-keyword (symbol-name name))))))

#++
(system-name (car (sexps-from-file (car (asd-files "./")))))
#++
(system-name '(defsystem foo))
