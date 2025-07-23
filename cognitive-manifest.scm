;; Guix manifest for cognitive ecosystem
;; Use with: guix install -m cognitive-manifest.scm

(specifications->manifest
  '(;; Core build tools
    "gcc-toolchain"
    "cmake"
    "make"
    "pkg-config"
    
    ;; Development dependencies
    "guile"
    "guile-dev"
    "python"
    "python-pip"
    
    ;; Cognitive framework dependencies
    "git"
    "curl"
    "wget"
    
    ;; OpenCog dependencies
    "boost"
    "cxxtest"
    
    ;; Meta-cognitive tools
    "emacs"
    "vim"))
