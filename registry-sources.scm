;; Registry Source Catalog - Cognitive Flowchart Initiation Node
;; Part of the OpenCog/Guix Cognitive Ecosystem Framework
;; 
;; This file defines registry URLs and metadata for package management
;; source discovery. It serves as the starting point for the cognitive
;; flowchart that enables seeding and managing distributed source registries.

(use-modules 
  (srfi srfi-1)
  (srfi srfi-9)
  (ice-9 match))

;; Registry node definition following cognitive grammar
(define-record-type registry-node
  (make-registry-node id url categories attributes tensor-shape metadata)
  registry-node?
  (id registry-node-id)
  (url registry-node-url)
  (categories registry-node-categories)
  (attributes registry-node-attributes)
  (tensor-shape registry-node-tensor-shape)
  (metadata registry-node-metadata))

;; Primary OpenCog GitHub registry (Initial Entry)
(define opencog-github-registry
  (make-registry-node
    "opencog-github"
    "https://github.com/opencog/*"
    '("AGI" "cognitive-architecture" "atomspace" "reasoning")
    '("public" "maintained" "open-source")
    '(1 . (registry_count url_complexity tag_cardinality))
    '((last-updated . "2024-01-01")
      (scan-frequency . "daily")
      (priority . "high")
      (api-endpoint . "https://api.github.com/orgs/opencog/repos"))))

;; Extended registries for ecosystem expansion
(define guix-packages-registry
  (make-registry-node
    "guix-packages"
    "https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages"
    '("package-management" "functional" "reproducible")
    '("official" "curated" "immutable")
    '(1 . (package_count complexity_level stability_index))
    '((last-updated . "2024-01-01")
      (scan-frequency . "weekly")
      (priority . "medium")
      (api-endpoint . "https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages"))))

(define julia-hub-registry
  (make-registry-node
    "julia-ecosystem"
    "https://github.com/JuliaLang/*"
    '("scientific-computing" "package-ecosystem" "performance")
    '("public" "maintained" "ecosystem")
    '(1 . (ecosystem_size complexity_metric performance_index))
    '((last-updated . "2024-01-01")
      (scan-frequency . "weekly")
      (priority . "medium")
      (api-endpoint . "https://api.github.com/orgs/JuliaLang/repos"))))

;; Registry catalog collection
(define registry-catalog
  (list opencog-github-registry
        guix-packages-registry
        julia-hub-registry))

;; Hypergraph representation function
(define (registry-node->hypergraph-repr node)
  "Convert a registry node to hypergraph representation"
  `(registry-node ,(registry-node-id node)
    (list 'URL ,(registry-node-url node))
    (list 'CATEGORIES ',(registry-node-categories node))
    (list 'ATTRIBUTES ',(registry-node-attributes node))
    (list 'TENSOR-SHAPE ,(registry-node-tensor-shape node))
    (list 'METADATA ',(registry-node-metadata node))))

;; Export all registries as hypergraph representations
(define (export-registry-hypergraph)
  "Export the complete registry catalog as hypergraph representations"
  (map registry-node->hypergraph-repr registry-catalog))

;; Registry query interface
(define (find-registry-by-id id)
  "Find a registry node by its identifier"
  (find (lambda (node) (string=? (registry-node-id node) id)) registry-catalog))

(define (filter-registries-by-category category)
  "Filter registries that contain the specified category"
  (filter (lambda (node) 
            (member category (registry-node-categories node))) 
          registry-catalog))

(define (get-active-registries)
  "Get all registries with high or medium priority"
  (filter (lambda (node)
            (let ((priority (assoc-ref (registry-node-metadata node) 'priority)))
              (member priority '("high" "medium"))))
          registry-catalog))

;; Tensor shape analysis for cognitive load assessment
(define (calculate-registry-complexity)
  "Calculate total cognitive complexity across all registries"
  (let ((shapes (map registry-node-tensor-shape registry-catalog)))
    (fold (lambda (shape acc)
            (match shape
              ((count . dimensions) (+ acc count))
              (_ acc)))
          0 shapes)))

;; Export public interface
(export registry-node
        make-registry-node
        registry-catalog
        export-registry-hypergraph
        find-registry-by-id
        filter-registries-by-category
        get-active-registries
        calculate-registry-complexity
        opencog-github-registry)