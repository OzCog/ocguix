;; Guix manifest for cognitive ecosystem
;; Use with: guix install -m cognitive-manifest.scm
;; Updated for madness issue #68 - includes AI model and KoboldCpp dependencies
;; Updated for issue #109 - includes cogutil vendoring dependencies

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
    "python-requests"  ; For KoboldCpp API integration
    
    ;; Cognitive framework dependencies
    "git"
    "curl"
    "wget"
    
    ;; OpenCog and cogutil dependencies
    "boost"
    "cxxtest"
    "binutils"         ; For cogutil build process
    "libtool"          ; For cogutil build process
    
    ;; AI/ML model infrastructure (issue #68, #74)
    "python-numpy"
    "python-scipy"
    "python-flask"      ; KoboldCpp web interface
    "python-fastapi"    ; Alternative API framework
    
    ;; Agent-zero integration dependencies (issue #70)
    "python-asyncio"
    "python-json"
    
    ;; Distributed cognitive grammar (issue #77)
    "guile-json"        ; JSON processing in Scheme
    "guile-web"         ; HTTP client capabilities
    
    ;; Documentation generation (issue #78)
    "graphviz"          ; For mermaid diagram rendering
    "pandoc"            ; Documentation processing
    
    ;; Meta-cognitive tools
    "emacs"
    "vim"
    
    ;; Container/workspace support (issue #69)
    "docker"
    "docker-compose"))
