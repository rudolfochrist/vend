(defpackage vend
  (:use :cl)
  (:local-nicknames (#:g #:simple-graph)
                    (#:p #:filepaths)
                    (#:t #:transducers))
  (:export #:main)
  (:documentation "Simply vendor your Common Lisp project dependencies."))

(in-package :vend)

#-ecl
(error "VEND can only be compiled with ECL.")

(defconstant +parents+
  '(:3bmd-ext-code-blocks :3bmd
    :3b-bmfont/json :3b-bmfont
    :alloy-animation :alloy
    :alloy-simple :alloy
    :alloy-simple-presentations :alloy
    :binpack/2 :binpack
    :cffi-grovel :cffi
    :cffi-toolchain :cffi
    :cl-aa :cl-vectors
    :cl-aa-misc :cl-vectors
    :cl-glu :cl-opengl
    :cl-glut :cl-opengl
    :cl-mixed-alsa :cl-mixed
    :cl-paths :cl-vectors
    :cl-paths-ttf :cl-vectors
    :cl-ppcre-unicode :cl-ppcre
    :cl-unicode/base :cl-unicode
    :depot-in-memory :depot
    :depot-virtual :depot
    :depot-zip :depot
    :dref :mgl-pax
    :feedback-client :feedback
    :machine-state/opengl :machine-state
    :mgl-pax-bootstrap :mgl-pax
    :north-drakma :north
    :plump-dom :plump
    :queues.priority-queue :queues
    :qtcore :qt+libs
    :qtgui :qt+libs
    :qtopengl :qt+libs
    :regression-test :ansi-test
    :rt :ansi-test
    :sdf/bmfont :sdf
    :transducers/fset :transducers
    :transducers/jzon :transducers
    :transducers/tests :transducers
    :trial-assets :trial-assets
    :trivia.balland2006 :trivia
    :trivia.level2 :trivia
    :trivia.trivial :trivia
    :uiop :asdf)
  "Systems are often bundled together into a single repository. This list helps
map back to the parent, such that later only one git clone is performed.")

(defun get-parent (sys)
  (or (getf +parents+ sys)
      (when (string-starts-with? (symbol-name sys) "TRIAL-") :trial)
      (when (string-starts-with? (symbol-name sys) "IRONCLAD/") :ironclad)
      sys))

(defun string-starts-with? (string prefix)
  (let ((pos (mismatch prefix string)))
    (or (null pos)
        (>= pos (length prefix)))))

#++
(string-starts-with? "trial-alloy" "trial-")
#++
(string-starts-with? "hello" "b")

(defconstant +exclude+
  '(;; Not hosted on any public forges.
    :cl-postgres
    :hu.dwim.presentation
    :hu.dwim.web-server
    :puri
    ;; Compiler Internals
    :sb-bsd-sockets
    :sb-cltl2
    :sb-posix
    :extensible-sequences
    ;; QT Confusion
    ;; :qtcore
    ;; :qtopengl
    ;; :phonon
    ;; CFFI?
    :devil
    ;; A Corman-specific dependency requested in Bordeaux Threads.
    :threads
    ;; Way more trouble than its worth.)
    :asdf)
  "Known naughty systems that we can't do anything about.")

(defconstant +sources+
  '(:3b-bmfont       "https://github.com/3b/3b-bmfont.git"
    :3b-hdr          "https://github.com/3b/3b-hdr.git"
    :3bmd            "https://github.com/3b/3bmd.git"
    :3bz             "https://github.com/3b/3bz.git"
    :3d-math         "https://github.com/Shinmera/3d-math.git"
    :3d-spaces       "https://github.com/Shirakumo/3d-spaces.git"
    :alexandria      "https://gitlab.common-lisp.net/alexandria/alexandria.git"
    :alloy           "https://github.com/Shirakumo/alloy.git"
    :anaphora        "https://github.com/spwhitton/anaphora.git"
    :ansi-test       "https://gitlab.common-lisp.net/ansi-test/ansi-test.git"
    :array-utils     "https://github.com/Shinmera/array-utils.git"
    :asdf            "https://gitlab.common-lisp.net/asdf/asdf.git"
    :atomics         "https://github.com/Shinmera/atomics.git"
    :babel           "https://github.com/cl-babel/babel.git"
    :bodge-heap      "https://github.com/borodust/bodge-heap.git"
    :bordeaux-threads "https://github.com/sionescu/bordeaux-threads.git"
    :binary-structures "https://github.com/Shinmera/binary-structures.git"
    :binpack         "https://github.com/lispgames/binpack.git"
    :cffi            "https://github.com/cffi/cffi.git"
    :checkl          "https://github.com/rpav/CheckL.git"
    :chipz           "https://github.com/sharplispers/chipz.git"
    :chunga          "https://github.com/edicl/chunga.git"
    :cl+ssl          "https://github.com/cl-plus-ssl/cl-plus-ssl.git"
    :cl-autowrap     "https://github.com/rpav/cl-autowrap.git"
    :cl-base64       "https://github.com/darabi/cl-base64.git"
    :cl-bmp          "https://github.com/Shinmera/cl-bmp.git"
    :cl-change-case  "https://github.com/rudolfochrist/cl-change-case.git"
    :cl-fad          "https://github.com/edicl/cl-fad.git"
    :cl-fbx          "https://github.com/Shirakumo/cl-fbx.git"
    :cl-fond         "https://github.com/Shirakumo/cl-fond.git"
    :cl-ftp          "https://github.com/pinterface/cl-ftp.git"
    :cl-gamepad      "https://github.com/Shirakumo/cl-gamepad.git"
    :cl-gltf         "https://github.com/Shirakumo/cl-gltf.git"
    :cl-gog-galaxy   "https://github.com/Shinmera/cl-gog-galaxy.git"
    :cl-jpeg         "https://github.com/sharplispers/cl-jpeg.git"
    :cl-json         "https://github.com/sharplispers/cl-json.git"
    :cl-ktx          "https://github.com/Shinmera/cl-ktx.git"
    :cl-l10n         "https://gitlab.common-lisp.net/cl-l10n/cl-l10n.git"
    :cl-markless     "https://github.com/Shirakumo/cl-markless.git"
    :cl-mixed        "https://github.com/Shirakumo/cl-mixed.git"
    :cl-mpg123       "https://github.com/Shirakumo/cl-mpg123.git"
    :cl-opengl       "https://github.com/3b/cl-opengl.git"
    :cl-opus         "https://github.com/Shirakumo/cl-opus.git"
    :cl-ppcre        "https://github.com/edicl/cl-ppcre.git"
    :cl-qoa          "https://github.com/Shinmera/cl-qoa.git"
    :cl-resvg        "https://github.com/Shirakumo/cl-resvg.git"
    :cl-steamworks   "https://github.com/Shinmera/cl-steamworks.git"
    :cl-tga          "https://github.com/fisxoj/cl-tga.git"
    :cl-theora       "https://github.com/Shirakumo/cl-theora.git"
    :cl-turbojpeg    "https://github.com/Shirakumo/cl-turbojpeg.git"
    :cl-unicode      "https://github.com/edicl/cl-unicode.git"
    :cl-vectors      "https://github.com/fjolliton/cl-vectors.git"
    :cl-vorbis       "https://github.com/Shirakumo/cl-vorbis.git"
    :cl-wavefront    "https://github.com/Shirakumo/cl-wavefront.git"
    :classowary      "https://github.com/Shinmera/classowary.git"
    :closer-mop      "https://github.com/pcostanza/closer-mop.git"
    :closure-common  "https://github.com/sharplispers/closure-common.git"
    :clss            "https://github.com/Shinmera/CLSS.git"
    :colored         "https://github.com/Shinmera/colored.git"
    :com.gigamonkeys.binary-data "https://github.com/gigamonkey/monkeylib-binary-data.git"
    :com.inuoe.jzon  "https://github.com/Zulu-Inuoe/jzon.git"
    :com-on          "https://github.com/Shinmera/com-on.git"
    :command-line-arguments "https://github.com/fare/command-line-arguments.git"
    :contextl        "https://github.com/pcostanza/contextl.git"
    :convex-covering "https://github.com/Shirakumo/convex-covering.git"
    :cxml            "https://github.com/sharplispers/cxml.git"
    :damn-fast-priority-queue "https://github.com/phoe/damn-fast-priority-queue.git"
    :deflate         "https://github.com/pmai/Deflate.git"
    :defpackage-plus "https://github.com/rpav/defpackage-plus.git"
    :deploy          "https://github.com/Shinmera/deploy.git"
    :depot           "https://github.com/Shinmera/depot.git"
    :dexador         "https://github.com/fukamachi/dexador.git"
    :dns-client      "https://github.com/Shinmera/dns-client.git"
    :dissect         "https://github.com/Shinmera/dissect.git"
    :documentation-utils "https://github.com/Shinmera/documentation-utils.git"
    :fare-quasiquote "https://gitlab.common-lisp.net/frideau/fare-quasiquote.git"
    :fare-utils      "https://gitlab.common-lisp.net/frideau/fare-utils.git"
    :fast-http       "https://github.com/fukamachi/fast-http.git"
    :fast-io         "https://github.com/rpav/fast-io.git"
    :feedback        "https://github.com/Shirakumo/feedback.git"
    :file-select     "https://github.com/Shinmera/file-select.git"
    :flow            "https://github.com/Shinmera/flow.git"
    :font-discovery  "https://github.com/Shinmera/font-discovery.git"
    :form-fiddle     "https://github.com/Shinmera/form-fiddle.git"
    :fiasco          "https://github.com/joaotavora/fiasco.git"
    :file-attributes "https://github.com/Shinmera/file-attributes.git"
    :file-notify     "https://github.com/Shinmera/file-notify.git"
    :filepaths       "https://codeberg.org/fosskers/filepaths.git"
    :filesystem-utils "https://github.com/Shinmera/filesystem-utils.git"
    :fiveam          "https://github.com/lispci/fiveam.git"
    :flexi-streams   "https://github.com/edicl/flexi-streams.git"
    :float-features  "https://github.com/Shinmera/float-features.git"
    :for             "https://github.com/Shinmera/for.git"
    :frugal-uuid     "https://github.com/ak-coram/cl-frugal-uuid.git"
    :fset            "https://gitlab.common-lisp.net/fset/fset.git"
    :glfw            "https://github.com/Shirakumo/glfw.git"
    :global-vars     "https://github.com/lmj/global-vars.git"
    :glop            "https://github.com/lispgames/glop.git"
    :glsl-toolkit    "https://github.com/Shirakumo/glsl-toolkit.git"
    :harmony         "https://github.com/Shirakumo/harmony.git"
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
    :hunchentoot     "https://github.com/edicl/hunchentoot.git"
    :ieee-floats     "https://github.com/marijnh/ieee-floats.git"
    :idna            "https://github.com/antifuchs/idna.git"
    :introspect-environment "https://github.com/Bike/introspect-environment.git"
    :iolib           "https://github.com/sionescu/iolib.git"
    :ironclad        "https://github.com/sharplispers/ironclad.git"
    :iterate         "https://gitlab.common-lisp.net/iterate/iterate.git"
    :jsown           "https://github.com/madnificent/jsown.git"
    :lambda-fiddle   "https://github.com/Shinmera/lambda-fiddle.git"
    :language-codes  "https://github.com/Shinmera/language-codes.git"
    :lift            "https://github.com/hraban/lift.git"
    :lisp-namespace  "https://github.com/guicho271828/lisp-namespace.git"
    :lisp-unit       "https://github.com/OdonataResearchLLC/lisp-unit.git"
    :local-time      "https://github.com/dlowe-net/local-time.git"
    :lparallel       "https://github.com/lmj/lparallel.git"
    :lquery          "https://github.com/Shinmera/lquery.git"
    :lru-cache       "https://github.com/Shinmera/lru-cache.git"
    :machine-state   "https://github.com/Shinmera/machine-state.git"
    :manifolds       "https://github.com/Shirakumo/manifolds.git"
    :memory-regions  "https://github.com/Shinmera/memory-regions.git"
    :messagebox      "https://github.com/Shinmera/messagebox.git"
    :metabang-bind   "https://github.com/hraban/metabang-bind.git"
    :mgl-pax         "https://github.com/melisgl/mgl-pax.git"
    :minheap         "https://github.com/sfrank/minheap.git"
    :misc-extensions "https://gitlab.common-lisp.net/misc-extensions/misc-extensions.git"
    :mmap            "https://github.com/Shinmera/mmap.git"
    :multilang-documentation "https://github.com/Shinmera/multilang-documentation.git"
    :named-readtables "https://github.com/melisgl/named-readtables.git"
    :nibbles         "https://github.com/sharplispers/nibbles.git"
    :north           "https://github.com/Shinmera/north.git"
    :open-with       "https://github.com/Shinmera/open-with.git"
    :opticl          "https://github.com/slyrus/opticl.git"
    :opticl-core     "https://github.com/slyrus/opticl-core.git"
    :optima          "https://github.com/m2ym/optima.git"
    :parachute       "https://github.com/Shinmera/parachute.git"
    :parse-float     "https://github.com/soemraws/parse-float.git"
    :parse-number    "https://github.com/sharplispers/parse-number.git"
    :pathname-utils  "https://github.com/Shinmera/pathname-utils.git"
    :pettomato-indexed-priority-queue "https://github.com/austinhaas/pettomato-indexed-priority-queue.git"
    :plump           "https://github.com/Shinmera/plump.git"
    :pileup          "https://github.com/nikodemus/pileup.git"
    :piping          "https://github.com/Shinmera/piping.git"
    :png-read        "https://github.com/Ramarren/png-read.git"
    :pngload         "https://github.com/3b/pngload.git"
    :precise-time    "https://github.com/Shinmera/precise-time.git"
    :priority-queue  "https://github.com/dsorokin/priority-queue.git"
    :proc-parse      "https://github.com/fukamachi/proc-parse.git"
    :promise         "https://github.com/Shinmera/promise.git"
    :punycode        "https://github.com/Shinmera/punycode.git"
    :pythonic-string-reader "https://github.com/smithzvk/pythonic-string-reader.git"
    :qbase64         "https://github.com/chaitanyagupta/qbase64.git"
    :queues          "https://github.com/oconnore/queues.git"
    :qoi             "https://github.com/bpanthi977/qoi.git"
    :qt+libs         "https://github.com/commonqt/commonqt.git"
    :qt-libs         "https://github.com/Shinmera/qt-libs.git"
    :qtools          "https://github.com/Shinmera/qtools.git"
    :quickhull       "https://github.com/Shirakumo/quickhull.git"
    :random-sampling "https://github.com/Shinmera/random-sampling.git"
    :random-state    "https://github.com/Shinmera/random-state.git"
    :raster          "https://github.com/Shirakumo/raster.git"
    :retrospectiff   "https://github.com/slyrus/retrospectiff.git"
    :salza2          "https://github.com/xach/salza2.git"
    :sdf             "https://github.com/lispgames/sdf.git"
    :sdl2            "https://github.com/lispgames/cl-sdl2.git"
    :sha3            "https://github.com/pmai/sha3.git"
    :simple-graph    "https://github.com/fosskers/simple-graph.git"
    :simple-tasks    "https://github.com/Shinmera/simple-tasks.git"
    :skippy          "https://github.com/xach/skippy.git"
    :smart-buffer    "https://github.com/fukamachi/smart-buffer.git"
    :split-sequence  "https://github.com/sharplispers/split-sequence.git"
    :static-vectors  "https://github.com/sionescu/static-vectors.git"
    :stealth-mixin   "https://github.com/robert-strandh/Stealth-mixin.git"
    :str             "https://github.com/vindarel/cl-str.git"
    :swank           "https://github.com/slime/slime.git"
    :swap-bytes      "https://github.com/sionescu/swap-bytes.git"
    :system-locale   "https://github.com/Shinmera/system-locale.git"
    :terrable        "https://github.com/Shirakumo/terrable.git"
    :text-draw       "https://github.com/Shinmera/text-draw.git"
    :transducers     "https://codeberg.org/fosskers/cl-transducers.git"
    :trial           "https://github.com/Shirakumo/trial.git"
    :trial-assets    "https://github.com/Shirakumo/trial-assets.git"
    :trivia          "https://github.com/guicho271828/trivia.git"
    :trivial-channels "https://github.com/rpav/trivial-channels.git"
    :trivial-cltl2   "https://github.com/Zulu-Inuoe/trivial-cltl2.git"
    :trivial-custom-debugger "https://github.com/phoe/trivial-custom-debugger.git"
    :trivial-deprecate "https://github.com/Shinmera/trivial-deprecate.git"
    :trivial-extensible-sequences "https://github.com/Shinmera/trivial-extensible-sequences.git"
    :trivial-garbage "https://github.com/trivial-garbage/trivial-garbage.git"
    :trivial-gray-streams "https://github.com/trivial-gray-streams/trivial-gray-streams.git"
    :trivial-features "https://github.com/trivial-features/trivial-features.git"
    :trivial-indent  "https://github.com/Shinmera/trivial-indent.git"
    :trivial-main-thread "https://github.com/Shinmera/trivial-main-thread.git"
    :trivial-mimes   "https://github.com/Shinmera/trivial-mimes.git"
    :trivial-ssh     "https://github.com/eudoxia0/trivial-ssh.git"
    :trivial-timeout "https://github.com/hraban/trivial-timeout.git"
    :try             "https://github.com/melisgl/try.git"
    :type-i          "https://github.com/guicho271828/type-i.git"
    :type-templates  "https://github.com/Shinmera/type-templates.git"
    :uax-14          "https://github.com/Shinmera/uax-14.git"
    :usocket         "https://github.com/usocket/usocket.git"
    :verbose         "https://github.com/Shinmera/verbose.git"
    :winhttp         "https://github.com/fjames86/winhttp.git"
    :xsubseq         "https://github.com/fukamachi/xsubseq.git"
    :zippy           "https://github.com/Shinmera/zippy.git"
    :zpb-exif        "https://github.com/xach/zpb-exif.git"
    :zpb-ttf         "https://github.com/xach/zpb-ttf.git"
    :zpng            "https://github.com/xach/zpng.git")
  "All actively depended-on Common Lisp libraries.")

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

(defun string-from-file (path)
  "Preserves newlines but removes whole-line comments."
  (t:transduce (t:comp (t:map (lambda (line) (string-trim " " line)))
                       (t:filter (lambda (line) (not (comment? line))))
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

(defun into-keyword (s)
  "Turn anything stringy or symboly into a keyword."
  (etypecase s
    (keyword s)
    (string (intern (string-upcase s) "KEYWORD"))
    (symbol (intern (symbol-name s) "KEYWORD"))))

#++
(into-keyword 'foo)

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

(defun clone (url path)
  "Given a source URL to clone from, do a shallow git clone into a given absolute PATH."
  (unless (probe-file path)
    (multiple-value-bind (stream code obj)
        (ext:run-program "git" (list "clone" "--depth=1" url path) :output t)
      (declare (ignore stream obj))
      (assert (= 0 code) nil "Clone failed: ~a" url))))

(defun work (cwd target)
  "Recursively perform a git clone on every detected dependency."
  (let ((graph  (g:make-graph))
        (cloned (make-hash-table)))
    (labels ((scan-systems (dir &key (shallow nil))
               (t:transduce (t:comp (t:log (lambda (acc item) (format t "[vend] Scanning: ~a~%" item)))
                                    (t:map #'sexps-from-file)
                                    #'t:concatenate
                                    (t:filter #'system?)
                                    (t:map (lambda (sys)
                                             (let ((name (system-name sys)))
                                               (format t "[vend] Analysing: ~a~%" name)
                                               (g:add-node! graph name)
                                               (dolist (dep (depends-from-system sys))
                                                 (g:add-node! graph dep)
                                                 (g:add-edge! graph name dep))
                                               name))))
                            #'t:cons (asd-files dir :shallow shallow)))
             (unique-leaves (g)
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
                   (clone url path)
                   (setf (gethash dep cloned) t)
                   (scan-systems path)
                   (dolist (leaf (unique-leaves (apply #'g:subgraph graph top)))
                     (recurse top leaf))))))
      (let* ((top  (scan-systems cwd :shallow t))
             (root (or (get-parent (car top)) (car top))))
        ;; This is the root project directory, so it's already considered "cloned".
        (setf (gethash root cloned) t)
        (dolist (leaf (unique-leaves graph))
          (recurse top leaf))
        ;; Clean the graph one final time so that the user doesn't need to see
        ;; ugly systems that weren't asked for.
        (apply #'g:subgraph graph top)))))

#++
(let* ((cwd #p"/home/colin/code/common-lisp/trial/")
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
  '(("--help" 0 (princ ext:*help-message*))
    ("--version" 0 (format t "0.1.0~%"))
    ("get"    0 (vend/get))
    ("graph"  0 (error "Not yet implemented!"))
    ("repl"   1 (vend/repl (rest 1)) :stop)))

(defun vend/get ()
  "Download all dependencies."
  (let* ((cwd (ext:getcwd))
         (dir (p:ensure-directory (p:join cwd "vendored"))))
    (cond ((probe-file dir)
           (format t "[vend] Target directory already exists.~%")
           (ext:quit 1))
          (t (work cwd dir)
             (format t "[vend] Done.~%")))))

(defun vend/repl (args)
  "Start a given repl."
  (let ((compiler (or (car args) "sbcl"))
        (load '("--eval" "(require :asdf)" "--eval" "(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))")))
    (ext:run-program compiler (append (cdr args) load) :output t :input *standard-input*)))

(defun main ()
  (let ((ext:*lisp-init-file-list* nil)
        (ext:*help-message* +help+))
    (ext:process-command-args :rules +vend-rules+)
    (ext:quit 0)))

;; Vendor Alloy
;; Vendor Kandria
;; Vendor ironclad
;; vend get --rmgit
;; vend open <foo>  <- opens project URL in browser

;; Bad boys:
;; https://github.com/slyrus/opticl/blob/master/opticl-doc.asd
;; https://github.com/rpav/fast-io/blob/master/fast-io-test.asd
;; https://github.com/sharplispers/ironclad/blob/master/ironclad.asd
;; https://github.com/sharplispers/chipz/blob/master/chipz.asd
