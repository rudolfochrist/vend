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

(defun root-asd-files (dir)
  "Yield the pathnames of all `.asd' outside of `vendored/'."
  (t:transduce (t:comp (t:filter (lambda (path)
                                   (let ((dir (car (last (p:components path)))))
                                     (and (not (string= ".git" dir))
                                          (not (string= "vendored" dir))
                                          (not (string= ".qlot" dir))))))
                       (t:map #'asd-files)
                       (t:once (asd-files dir :shallow t))
                       #'t:concatenate)
               #'t:cons (directory (p:ensure-directory (p:join dir "*/")))))

#++
(root-asd-files (ext:getcwd))
#++
(root-asd-files "/home/colin/code/common-lisp/lem/")

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
  (t:transduce (t:comp (t:filter (lambda (line) (not (comment? line))))
                       (t:filter (lambda (line) (not (chipz? (string-left-trim " " line)))))
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

(defun systems-from-file (path)
  "Extract all `defsystem' forms as proper sexp from a file."
  (t:transduce (t:map (lambda (sys)
                        (let* ((clean (sanitize sys))
                               (stream (make-string-input-stream clean)))
                          (read stream nil :eof))))
               #'t:cons (all-system-strings (string-from-file path))))

#++
(systems-from-file (car (asd-files "./")))

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

(defun other-reader? (chars)
  (and (eql #\# (nth 0 chars))
       (eql #\. (nth 1 chars))))

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

(defun command-asdf-call? (chars)
  (and (eql #\space (nth 0 chars))
       (eql #\a (nth 1 chars))
       (eql #\s (nth 2 chars))
       (eql #\d (nth 3 chars))
       (eql #\f (nth 4 chars))
       (eql #\: (nth 5 chars))))

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
           (eql #\m (nth 4 chars))
           (eql #\v (nth 4 chars)))))

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

(defun gendoc? (chars)
  (and (eql #\( (nth 0 chars))
       (eql #\g (nth 1 chars))
       (eql #\e (nth 2 chars))
       (eql #\n (nth 3 chars))
       (eql #\d (nth 4 chars))
       (eql #\o (nth 5 chars))
       (eql #\c (nth 6 chars))
       (eql #\: (nth 7 chars))))

(defun at-defsystem? (str ix)
  "Does the string at the given index start with a defsystem form?"
  (and (or (string-starts-with? str "(defsystem " :from ix)
           (string-starts-with? str "(asdf:defsystem " :from ix))
       (or (zerop ix)
           ;; Due to Ironclad's ridiculous ASD file.
           (eql #\newline (aref str (1- ix))))))

#++
(at-defsystem? "(defsystem :foo)" 0)
#++
(at-defsystem? " (defsystem :foo)" 1)
#++
(at-defsystem? " (asdf:defsystem :foo)" 1)
#++
(at-defsystem? "(push 1 2)" 0)
#++
(at-defsystem? "`(defsystem :foo)" 1)

(defun extract-defsystem (str ix)
  "Given a starting index into a string, and the knowledge that a `defsystem' form
starts there, extract that substring. As a second return value, yields the index
to continue from."
  (let ((len (length str)))
    (labels ((find-closing-paren (parens curr-ix)
               (let ((char (aref str curr-ix)))
                 (cond ((>= curr-ix len) nil)
                       ((eql #\( char) (find-closing-paren (1+ parens) (1+ curr-ix)))
                       ((and (eql #\) char) (= 1 parens)) curr-ix)
                       ((eql #\) char) (find-closing-paren (1- parens) (1+ curr-ix)))
                       (t (find-closing-paren parens (1+ curr-ix)))))))
      (let ((end (find-closing-paren 1 (1+ ix))))
        (when end
          (values (subseq str ix (1+ end))
                  (1+ end)))))))

#++
(extract-defsystem "(defsystem :foo :depends-on (:a :b :c)) (next)" 0)
#++
(extract-defsystem " (defsystem :foo :depends-on (:a :b :c)) (next)" 1)
#++
(extract-defsystem "" 0)

;; TODO: 2025-01-16 Get around `ironclad' doing a `defsystem' inside of a macro.
(defun all-system-strings (str)
  "Extract all `defsystem' forms as substrings from some parent string."
  (let ((len (length str)))
    (labels ((recurse (acc ix)
               (cond ((>= ix len) acc)
                     ((at-defsystem? str ix)
                      (multiple-value-bind (sub next) (extract-defsystem str ix)
                        (recurse (cons sub acc) next)))
                     (t (recurse acc (1+ ix))))))
      (nreverse (recurse '() 0)))))

#++
(all-system-strings "(foo) (defsystem :bar) (push 1 2) (defsystem :baz) t")
#++
(all-system-strings "")

(defun sanitize (str)
  "Remove and/or replace a number of naughty forms that prevent `read' from
succeeding as-is on a given string."
  (labels ((keep (acc chars)
             (let ((head (car chars))
                   (tail (cdr chars)))
               (cond ((null head) acc)
                     ;; The presence of read-time macros confuses `read', so we
                     ;; proactively remove them.
                     ((reader-macro? chars)
                      (keep (cons #\t acc) (chuck 1 (cddr tail))))
                     ;; ((other-reader? chars)
                     ;;  (keep acc (cdr tail)))
                     ;; ;; Likewise, some clever package authors like to utilise
                     ;; ;; `asdf' and `uiop' directly in their system definitions.
                     ;; ;; This similarly causes problems with `read', so we remove
                     ;; ;; such calls.
                     ((or (asdf-call? chars)
                          (uiop-call? chars))
                      (keep (cons #\( acc) (nthcdr 5 tail)))
                     ((quoted-asdf-call? chars)
                      (keep (cons #\' acc) (nthcdr 5 tail)))
                     ;; ((command-asdf-call? chars)
                     ;;  (keep (cons #\space acc) (nthcdr 5 tail)))
                     ((grovel-call? chars)
                      (keep (cons #\t acc) (chuck 1 (cddr tail))))
                     ;; ((def? chars)
                     ;;  (keep (cons #\t acc) (chuck 1 (cddr tail))))
                     ((checkl? chars)
                      (keep (cons #\t acc) (chuck 1 (cddr tail))))
                     ;; ((eval-when? chars)
                     ;;  (keep (cons #\t acc) (chuck 1 (cddr tail))))
                     ;; ((asdf-/-call? chars)
                     ;;  (keep (cons #\( acc) (until-colon tail)))
                     ;; ((gendoc? chars)
                     ;;  (keep (cons #\( acc) (nthcdr 7 tail)))
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
(sanitize "(asdf:defsystem :foo
:long-description #.(+ 1 1)
:foo (asdf:bar)
:baz (cffi-grovel:grovel-file 1)
:beep (checkl:tests 1))")

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
                     ((other-reader? chars)
                      (keep acc (cdr tail)))
                     ;; Likewise, some clever package authors like to utilise
                     ;; `asdf' and `uiop' directly in their system definitions.
                     ;; This similarly causes problems with `read', so we remove
                     ;; such calls.
                     ((or (asdf-call? chars)
                          (uiop-call? chars))
                      (keep (cons #\( acc) (nthcdr 5 tail)))
                     ((quoted-asdf-call? chars)
                      (keep (cons #\' acc) (nthcdr 5 tail)))
                     ((command-asdf-call? chars)
                      (keep (cons #\space acc) (nthcdr 5 tail)))
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
                     ((gendoc? chars)
                      (keep (cons #\( acc) (nthcdr 7 tail)))
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
(remove-reader-chars " asdf:foo")
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
#++
(remove-reader-chars "(defvar blah 1)")
#++
(remove-reader-chars "(gendoc:define-gendoc-load-op blah 1)")
#++
(remove-reader-chars "#.*foo*")

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
                                  ;; HACK: To account for Radiance's
                                  ;; higher-level implementation injection
                                  ;; mechanism. `:interface' is otherwise not a
                                  ;; keyword supported by ASDF.
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
