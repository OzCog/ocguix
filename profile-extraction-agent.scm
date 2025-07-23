;; Profile Extraction Agent - Cognitive Flowchart Node 2
;; Part of the OpenCog/Guix Cognitive Ecosystem Framework
;;
;; This agent extracts build profiles and their manifests from registries,
;; implementing the profile extraction functionality specified in the 
;; cognitive flowchart design.

(use-modules 
  (srfi srfi-1)
  (ice-9 match)
  (ice-9 format)
  (ice-9 textual-ports)
  (json))

;; Load build profile definitions from base-devcontainers.scm
(load "base-devcontainers.scm")

;; Profile extraction implementation as specified in the issue
(define (extract-build-profiles registry-list)
  "Extract build profiles for each registry and write per-registry JSON files"
  (map (lambda (registry)
         (let* ((profiles (get-profiles-for-registry registry))
                (profile-data (map profile-to-json-data profiles))
                (tensor-shape `(,(length profiles) "profile_count" "feature_count" "build_time"))
                (output-filename (string-append (registry-id-from-registry registry) "-profiles.json"))
                (output-data `((generated . ,(current-time-iso))
                              (registry_id . ,(registry-id-from-registry registry))
                              (profiles . ,profile-data)
                              (tensor_shape . ,tensor-shape)
                              (summary . ((total_profiles . ,(length profiles))
                                         (ready_profiles . ,(count-ready-profiles profiles))
                                         (guix_reproducible . #t)
                                         (cognitive_complexity . ,(calculate-profiles-complexity profiles)))))))
           (write-json output-filename output-data)
           (format #t "📋 Profiles for ~a written to ~a~%" 
                   (registry-id-from-registry registry) output-filename)
           profiles))
       registry-list))

;; Helper to extract registry ID from registry object
(define (registry-id-from-registry registry)
  "Extract ID from registry node"
  (if (registry-node? registry)
      (registry-node-id registry)
      (string registry)))

;; Get profiles associated with a registry
(define (get-profiles-for-registry registry)
  "Get build profiles relevant to a specific registry"
  (let ((registry-id (registry-id-from-registry registry)))
    (match registry-id
      ("opencog-github" 
       (list opencog-dev-profile 
             atomspace-minimal-profile 
             cognitive-agent-profile 
             research-experimental-profile))
      ("guix-packages" 
       (list opencog-dev-profile 
             atomspace-minimal-profile))
      ("julia-ecosystem" 
       (list research-experimental-profile))
      (_ build-profile-catalog))))

;; Convert profile to JSON-compatible data
(define (profile-to-json-data profile)
  "Convert a build profile to JSON-compatible alist"
  `((id . ,(build-profile-id profile))
    (name . ,(build-profile-name profile))
    (description . ,(build-profile-description profile))
    (base_os . ,(build-profile-base-os profile))
    (features . ,(build-profile-features profile))
    (packages . ,(build-profile-packages profile))
    (guix_variants . ,(build-profile-guix-variants profile))
    (tensor_shape . ,(build-profile-tensor-shape profile))
    (status . "ready")
    (last_updated . ,(current-time-iso))))

;; Count ready profiles
(define (count-ready-profiles profiles)
  "Count profiles that are in ready state"
  (length profiles)) ; All profiles are considered ready for now

;; Calculate complexity for profile list
(define (calculate-profiles-complexity profiles)
  "Calculate total complexity across given profiles"
  (fold (lambda (profile acc)
          (match (build-profile-tensor-shape profile)
            ((count . dimensions) (+ acc count))
            (_ acc)))
        0 profiles))

;; Generate comprehensive build profile scan
(define (generate-build-profiles-scan)
  "Generate a comprehensive scan of all available build profiles"
  (let* ((all-profiles build-profile-catalog)
         (profile-data (map profile-to-json-data all-profiles))
         (tensor-shape `(,(length all-profiles) "profile_count" "feature_count" "build_time"))
         (output-data `((generated . ,(current-time-iso))
                       (profiles . ,profile-data)
                       (tensor_shape . ,tensor-shape)
                       (summary . ((total_profiles . ,(length all-profiles))
                                  (ready_profiles . ,(length all-profiles))
                                  (guix_reproducible . #t)
                                  (cognitive_complexity . ,(calculate-profile-complexity)))))))
    (write-json "build_profiles_scan.json" output-data)
    (format #t "📋 Complete profile scan written to build_profiles_scan.json~%")
    all-profiles))

;; Profile analysis utilities
(define (analyze-profile-features)
  "Analyze features across all profiles"
  (let ((all-features (append-map build-profile-features build-profile-catalog)))
    (map (lambda (feature)
           `((feature . ,feature)
             (usage_count . ,(count (lambda (f) (string=? f feature)) all-features))))
         (delete-duplicates all-features))))

;; Profile compatibility analysis
(define (check-profile-compatibility profile1 profile2)
  "Check compatibility between two profiles"
  (let ((features1 (build-profile-features profile1))
        (features2 (build-profile-features profile2))
        (os1 (build-profile-base-os profile1))
        (os2 (build-profile-base-os profile2)))
    `((compatible_os . ,(string=? os1 os2))
      (shared_features . ,(lset-intersection string=? features1 features2))
      (unique_features_1 . ,(lset-difference string=? features1 features2))
      (unique_features_2 . ,(lset-difference string=? features2 features1)))))

;; Helper to get current time in ISO format
(define (current-time-iso)
  "Get current time in ISO 8601 format"
  (strftime "%Y-%m-%dT%H:%M:%SZ" (gmtime (current-time))))

;; JSON writing utility
(define (write-json filename data)
  "Write data structure to JSON file"
  (call-with-output-file filename
    (lambda (port)
      (scm->json data port))))

;; Export main interface
(export extract-build-profiles
        generate-build-profiles-scan
        analyze-profile-features
        check-profile-compatibility
        profile-to-json-data)

;; Main execution when run as script
(when (defined? 'command-line)
  (let ((args (command-line)))
    (when (and (> (length args) 1)
               (string=? (cadr args) "--extract"))
      (format #t "🔧 Starting profile extraction...~%")
      (let ((scan-result (generate-build-profiles-scan)))
        (format #t "✅ Profile extraction complete. Found ~a profiles.~%" (length scan-result))
        (format #t "📋 Comprehensive scan written to build_profiles_scan.json~%")))))