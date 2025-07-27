;; guix.scm - Build recipe for ocguix with vendored cogutil
;; This file defines how to build the ocguix project using Guix
;; It expects cogutil to be vendored locally in ./cogutil

(use-modules (guix packages)
             (guix build-system cmake)
             (guix build-system gnu)
             (guix build-system trivial)
             (guix gexp)
             (gnu packages)
             (gnu packages base)
             (gnu packages bash)
             (gnu packages cmake)
             (gnu packages cpp)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages python)
             (gnu packages version-control))

(define cogutil-vendored
  ;; Local vendored cogutil package definition
  (package
    (name "cogutil-vendored")
    (version "2.0.4-vendored")
    (source (file-append (current-source-directory) "/cogutil"))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #t
       #:cmake ,cmake
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-source-dir
           (lambda _
             ;; The source is already in the correct place for vendored cogutil
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "make" "test")))))))
    (native-inputs
     (list cmake pkg-config))
    (inputs
     (list boost guile-3.0))
    (synopsis "Vendored OpenCog utilities library")
    (description
     "This is a vendored version of the OpenCog utilities library (cogutil)
that provides basic utilities and infrastructure for OpenCog projects.
This package builds from the local vendored copy in ./cogutil.")
    (home-page "https://github.com/opencog/cogutil")
    (license #f))) ; Will be inherited from cogutil

(define ocguix-package
  ;; Main ocguix package that includes the vendored cogutil
  (package
    (name "ocguix")
    (version "1.0.0")
    (source (current-source-directory))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source (assoc-ref %build-inputs "source"))
               (output (assoc-ref %outputs "out")))
           ;; Copy source files to output
           (copy-recursively source output)
           ;; Ensure cogutil is present
           (unless (file-exists? (string-append output "/cogutil/CMakeLists.txt"))
             (error "cogutil/CMakeLists.txt not found! Vendoring failed."))
           ;; Mark as executable the scripts
           (for-each (lambda (script)
                       (when (file-exists? (string-append output "/" script))
                         (chmod (string-append output "/" script) #o755)))
                     '("ocpkg" "octool-wip" "octool_rpi.sh" 
                       "guix-cognitive-bootstrap.sh"
                       "test-cognitive-flowchart.sh"))
           #t))))
    (propagated-inputs
     (list cogutil-vendored
           bash
           git
           cmake
           pkg-config
           guile-3.0
           python))
    (synopsis "OpenCog Guix package management and cognitive ecosystem")
    (description
     "OcGuix provides a collection of scripts and configurations for setting up
OpenCog development environments using Guix package manager. It includes a
complete cognitive ecosystem integrating KoboldCpp, Agent-Zero, distributed
cognitive grammar network processing, OpenCog AtomSpace, and Guix-based
reproducible environments. This package includes a vendored copy of cogutil.")
    (home-page "https://github.com/OzCog/ocguix")
    (license #f))) ; Mixed licenses

;; Export the main package
ocguix-package