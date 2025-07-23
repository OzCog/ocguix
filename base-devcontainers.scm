;; Baseline Build Profiles - Cognitive Flowchart Foundation Node
;; Part of the OpenCog/Guix Cognitive Ecosystem Framework
;;
;; This file defines baseline devcontainers and build profiles focused on
;; Guix reproducibility for typical OpenCog/Guix environments. Each profile
;; is tagged with supported features, OS requirements, and essential packages.

(use-modules 
  (srfi srfi-1)
  (ice-9 match))

;; Build profile definition following cognitive grammar
(define-record-type build-profile
  (make-build-profile id name description base-os features packages guix-variants tensor-shape)
  build-profile?
  (id build-profile-id)
  (name build-profile-name)
  (description build-profile-description)
  (base-os build-profile-base-os)
  (features build-profile-features)
  (packages build-profile-packages)
  (guix-variants build-profile-guix-variants)
  (tensor-shape build-profile-tensor-shape))

;; Core OpenCog development profile
(define opencog-dev-profile
  (make-build-profile
    "opencog-dev"
    "OpenCog Development Environment"
    "Complete development environment for OpenCog with AtomSpace, Cogutil, and language bindings"
    "guix-system"
    '("atomspace" "reasoning" "nlp" "python-bindings" "scheme-bindings" "debugging")
    '("gcc-toolchain" "cmake" "pkg-config" "boost" "cxxtest" "guile" "python" 
      "opencog-atomspace" "opencog-cogutil" "opencog-opencog" "gdb" "valgrind")
    '("stable" "latest" "development")
    '(10 . (feature_count package_complexity build_time))))

;; Minimal AtomSpace profile
(define atomspace-minimal-profile
  (make-build-profile
    "atomspace-minimal"
    "AtomSpace Minimal Environment"
    "Lightweight environment for AtomSpace development and experimentation"
    "guix-system"
    '("atomspace" "scheme-bindings" "basic-reasoning")
    '("gcc-toolchain" "cmake" "boost" "guile" "opencog-atomspace" "opencog-cogutil")
    '("stable" "latest")
    '(6 . (feature_count package_complexity build_time))))

;; Cognitive agent profile
(define cognitive-agent-profile
  (make-build-profile
    "cognitive-agent"
    "Cognitive Agent Runtime"
    "Runtime environment for deployed cognitive agents with minimal dependencies"
    "guix-system"
    '("runtime" "agent-execution" "minimal-footprint")
    '("guile" "opencog-atomspace" "opencog-cogutil" "opencog-opencog")
    '("stable")
    '(4 . (feature_count package_complexity build_time))))

;; Research experimentation profile
(define research-experimental-profile
  (make-build-profile
    "research-experimental"
    "Research Experimental Environment"
    "Cutting-edge environment with latest packages for research and experimentation"
    "guix-system"
    '("experimental" "research" "jupyter" "visualization" "machine-learning")
    '("gcc-toolchain" "cmake" "python" "jupyter" "matplotlib" "numpy" "scipy" 
      "opencog-atomspace" "opencog-cogutil" "opencog-opencog" "opencog-moses")
    '("latest" "development" "experimental")
    '(15 . (feature_count package_complexity build_time))))

;; Docker containerized profile
(define docker-cognitive-profile
  (make-build-profile
    "docker-cognitive"
    "Docker Cognitive Container"
    "Containerized cognitive environment for deployment and distribution"
    "docker"
    '("containerized" "portable" "reproducible" "cloud-deployment")
    '("gcc-toolchain" "cmake" "opencog-atomspace" "opencog-cogutil" "opencog-opencog")
    '("stable" "latest")
    '(8 . (feature_count package_complexity build_time))))

;; Build profile catalog
(define build-profile-catalog
  (list opencog-dev-profile
        atomspace-minimal-profile
        cognitive-agent-profile
        research-experimental-profile
        docker-cognitive-profile))

;; Guix manifest generation
(define (profile->guix-manifest profile)
  "Convert a build profile to a Guix manifest specification"
  `(specifications->manifest 
     ',(build-profile-packages profile)))

(define (profile->dockerfile profile)
  "Generate basic Dockerfile content for a build profile"
  (string-append
    "# Generated Dockerfile for " (build-profile-name profile) "\n"
    "FROM guix/guix:latest\n"
    "LABEL description=\"" (build-profile-description profile) "\"\n"
    "RUN guix pull && guix package -i " 
    (string-join (build-profile-packages profile) " ") "\n"
    "# Features: " (string-join (build-profile-features profile) ", ") "\n"))

;; Hypergraph representation
(define (profile->hypergraph-repr profile)
  "Convert a build profile to hypergraph representation"
  `(build-profile ,(build-profile-id profile)
    (list 'NAME ,(build-profile-name profile))
    (list 'DESCRIPTION ,(build-profile-description profile))
    (list 'BASE-OS ,(build-profile-base-os profile))
    (list 'FEATURES ',(build-profile-features profile))
    (list 'PACKAGES ',(build-profile-packages profile))
    (list 'GUIX-VARIANTS ',(build-profile-guix-variants profile))
    (list 'TENSOR-SHAPE ,(build-profile-tensor-shape profile))))

;; Export all profiles as hypergraph representations
(define (export-profiles-hypergraph)
  "Export the complete profile catalog as hypergraph representations"
  (map profile->hypergraph-repr build-profile-catalog))

;; Profile query interface
(define (find-profile-by-id id)
  "Find a build profile by its identifier"
  (find (lambda (profile) (string=? (build-profile-id profile) id)) build-profile-catalog))

(define (filter-profiles-by-feature feature)
  "Filter profiles that support the specified feature"
  (filter (lambda (profile) 
            (member feature (build-profile-features profile))) 
          build-profile-catalog))

(define (get-profiles-by-os os)
  "Get all profiles for the specified operating system"
  (filter (lambda (profile) 
            (string=? (build-profile-base-os profile) os))
          build-profile-catalog))

(define (calculate-profile-complexity)
  "Calculate total complexity across all build profiles"
  (let ((shapes (map build-profile-tensor-shape build-profile-catalog)))
    (fold (lambda (shape acc)
            (match shape
              ((count . dimensions) (+ acc count))
              (_ acc)))
          0 shapes)))

;; Profile recommendation engine
(define (recommend-profile-for-use-case use-case)
  "Recommend build profiles based on use case"
  (match use-case
    ("development" (list opencog-dev-profile))
    ("minimal" (list atomspace-minimal-profile cognitive-agent-profile))
    ("research" (list research-experimental-profile opencog-dev-profile))
    ("deployment" (list cognitive-agent-profile docker-cognitive-profile))
    ("experimentation" (list research-experimental-profile))
    (_ build-profile-catalog)))

;; Manifest export functions
(define (export-all-manifests)
  "Export Guix manifests for all profiles"
  (map (lambda (profile)
         (cons (build-profile-id profile)
               (profile->guix-manifest profile)))
       build-profile-catalog))

;; Export public interface
(export build-profile
        make-build-profile
        build-profile-catalog
        profile->guix-manifest
        profile->dockerfile
        export-profiles-hypergraph
        find-profile-by-id
        filter-profiles-by-feature
        get-profiles-by-os
        calculate-profile-complexity
        recommend-profile-for-use-case
        export-all-manifests
        opencog-dev-profile)