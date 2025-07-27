;; .gitpod/manifest.scm - Gitpod-optimized Guix package manifest
;; For OpenCog Cognitive Ecosystem deployment in cloud environments
;; 
;; Optimized for:
;; - Faster builds in Gitpod cloud environment
;; - Minimal dependencies for core functionality
;; - Fallback-friendly package selection
;; - Network-efficient downloads

(use-modules (gnu packages)
             (gnu packages base)
             (gnu packages bash)
             (gnu packages build-tools)
             (gnu packages cmake)
             (gnu packages cpp)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages python)
             (gnu packages python-xyz)
             (gnu packages version-control)
             (gnu packages curl)
             (gnu packages networking)
             (gnu packages documentation)
             (gnu packages boost)
             (gnu packages check))

(define gitpod-optimized-manifest
  ;; Optimized package list for Gitpod cloud environment
  ;; Prioritizes essential packages with minimal build times
  (specifications->manifest
    '(;; Essential build tools (fast to install)
      "gcc-toolchain@10"         ; Specific version for compatibility
      "cmake"                    ; Core build system
      "make"                     ; Build automation
      "pkg-config"               ; Configuration helper
      "git"                      ; Version control
      
      ;; Core development dependencies
      "guile"                    ; Scheme interpreter for cognitive agents
      "python"                   ; Python runtime
      "python-pip"               ; Python package manager
      
      ;; OpenCog core dependencies (minimal set)
      "boost"                    ; C++ libraries
      "cxxtest"                  ; C++ testing framework
      
      ;; Essential utilities
      "curl"                     ; Network requests
      "wget"                     ; File downloads
      "bash"                     ; Shell scripting
      
      ;; Python packages for cognitive ecosystem
      "python-requests"          ; HTTP library for KoboldCpp API
      "python-flask"             ; Web framework
      "python-numpy"             ; Numerical computing
      
      ;; Documentation and visualization (lightweight)
      "graphviz"                 ; Graph visualization
      
      ;; Network tools for distributed cognitive agents
      "netcat"                   ; Network utility
      
      ;; Development tools
      "emacs-minimal"            ; Text editor (minimal version)
      
      ;; Container support for advanced features
      "docker")))                ; Container runtime

;; Export the manifest for use with 'guix install -m manifest.scm'
gitpod-optimized-manifest