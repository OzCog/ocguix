(define-module (opencog-package))

(use-modules (guix packages)
             (guix git-download)
             (guix build-system gnu)
             (guix build-system cmake)
             (gnu packages base)
             (gnu packages cpp)
             (gnu packages cmake)
             (gnu packages pkg-config)
             (gnu packages boost)
             (gnu packages guile)
             (gnu packages python)
             (gnu packages version-control)
             ((guix licenses) #:prefix license:))

(define-public opencog
  (package
    (name "opencog")
    (version "latest-git")
    (source (git-checkout
             (url "https://github.com/opencog/opencog.git")
             (commit "HEAD")))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #t
       #:configure-flags
       '("-DCMAKE_BUILD_TYPE=Release"
         "-DBUILD_SHARED_LIBS=ON")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'setup-environment
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "GUILE_AUTO_COMPILE" "0")
             #t)))))
    (native-inputs
     (list cmake pkg-config))
    (inputs
     (list boost
           guile-3.0
           python))
    (synopsis "OpenCog AGI Framework")
    (description "The OpenCog cognitive architecturing toolkit provides a
framework for developing artificial general intelligence systems.")
    (home-page "https://opencog.org/")
    (license license:gpl3+)))

;; Also define cogutil dependency
(define-public cogutil
  (package
    (name "cogutil")
    (version "latest-git")
    (source (git-checkout
             (url "https://github.com/opencog/cogutil.git")
             (commit "HEAD")))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #t
       #:configure-flags
       '("-DCMAKE_BUILD_TYPE=Release")))
    (native-inputs
     (list cmake pkg-config))
    (inputs
     (list boost guile-3.0))
    (synopsis "OpenCog utilities library")
    (description "Cogutil provides basic utilities and infrastructure for
OpenCog projects.")
    (home-page "https://github.com/opencog/cogutil")
    (license license:gpl3+)))

;; Export the packages
opencog