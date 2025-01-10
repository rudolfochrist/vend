;;; All Common Lisp packages that are actively depended on in the ecosystem.

(in-package :vend)

;; TODO: 2024-01-11 Make this a HashTable.
(defparameter +parents+
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
    :cl-paths :cl-vectors
    :cl-paths-ttf :cl-vectors
    :cl-plus-c :cl-autowrap
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
  "systems are often bundled together into a single repository. this list helps
map back to the parent, such that later only one git clone is performed.")

(defparameter +exclude+
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

;; TODO: 2024-01-11 Make this a HashTable.
(defparameter +sources+
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
    :cl-cookie       "https://github.com/fukamachi/cl-cookie.git"
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
    :cl-utilities    "https://gitlab.common-lisp.net/cl-utilities/cl-utilities.git"
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
    :drakma          "https://github.com/edicl/drakma.git"
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
    :quri            "https://github.com/fukamachi/quri.git"
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

#++
(/ (length +sources+) 2)

(defun get-parent (sys)
  (or (getf +parents+ sys)
      (when (string-starts-with? (symbol-name sys) "TRIAL-")    :trial)
      (when (string-starts-with? (symbol-name sys) "IRONCLAD/") :ironclad)
      (when (string-starts-with? (symbol-name sys) "CL-MIXED-") :cl-mixed)
      (when (string-starts-with? (symbol-name sys) "ALLOY-")    :alloy)
      (when (string-starts-with? (symbol-name sys) "MEMORY-REGIONS/") :memory-regions)
      sys))
