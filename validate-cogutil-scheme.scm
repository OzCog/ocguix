#!/usr/bin/env guile
!#
;; Hypergraph Encoding: Scheme Cognitive Representation for Cogutil Vendoring
;; As specified in the issue requirements

(use-modules (ice-9 format)
             (ice-9 ftw)
             (srfi srfi-1))

(define (ensure-cogutil-vendored)
  "Ensure cogutil is vendored into the current directory"
  (display "🧠 Cognitive validation: Ensuring cogutil vendored...\n")
  
  (unless (file-exists? "cogutil")
    (display "📦 Vendoring cogutil from upstream...\n")
    (system "git clone https://github.com/opencog/cogutil.git cogutil")
    (system "rm -rf cogutil/.git"))
  
  (unless (file-exists? "cogutil/CMakeLists.txt")
    (error "❌ CMakeLists.txt missing in cogutil: vendoring failed!"))
  
  (display "✅ Cogutil vendoring validation passed\n"))

(define (analyze-cogutil-structure)
  "Analyze the structure of vendored cogutil for cognitive hypergraph"
  (display "🔍 Analyzing cogutil structure...\n")
  
  (let ((files (if (file-exists? "cogutil")
                   (length (find-files "cogutil" #:directories? #f))
                   0))
        (cmake-exists (file-exists? "cogutil/CMakeLists.txt"))
        (opencog-dir-exists (file-exists? "cogutil/opencog"))
        (git-removed (not (file-exists? "cogutil/.git"))))
    
    (format #t "📊 Cogutil Structure Analysis:\n")
    (format #t "   Files: ~a\n" files)
    (format #t "   CMakeLists.txt: ~a\n" (if cmake-exists "Present" "Missing"))
    (format #t "   OpenCog directory: ~a\n" (if opencog-dir-exists "Present" "Missing"))
    (format #t "   Git removed: ~a\n" (if git-removed "Yes" "No"))
    
    ;; Create cognitive hypergraph representation
    (let ((cognitive-node 
           `(cognitive-node "cogutil-vendored"
                           (file-count ,files)
                           (cmake-present ,cmake-exists)
                           (opencog-dir-present ,opencog-dir-exists)
                           (git-removed ,git-removed)
                           (tensor-shape ,(list files 
                                               (if cmake-exists 1 0)
                                               (if opencog-dir-exists 1 0)
                                               (if git-removed 1 0))))))
      
      (format #t "🧠 Hypergraph representation:\n")
      (format #t "~s\n" cognitive-node)
      
      cognitive-node)))

(define (validate-guix-environment)
  "Validate that we're in a proper Guix environment for building"
  (display "🔧 Validating Guix environment...\n")
  
  (let ((guix-available (zero? (system "command -v guix >/dev/null 2>&1")))
        (manifest-exists (file-exists? "cognitive-manifest.scm"))
        (build-recipe-exists (file-exists? "guix.scm")))
    
    (format #t "📋 Guix Environment Status:\n")
    (format #t "   Guix available: ~a\n" (if guix-available "Yes" "No"))
    (format #t "   Manifest present: ~a\n" (if manifest-exists "Yes" "No"))
    (format #t "   Build recipe present: ~a\n" (if build-recipe-exists "Yes" "No"))
    
    (when (not manifest-exists)
      (display "⚠️ WARNING: cognitive-manifest.scm not found\n"))
    
    (when (not build-recipe-exists)
      (display "⚠️ WARNING: guix.scm not found\n"))
    
    (and manifest-exists build-recipe-exists)))

(define (main args)
  "Main cognitive validation function"
  (display "🚀 Cognitive Vendoring Validation System\n")
  (display "=========================================\n")
  
  (catch 'system-error
    (lambda ()
      ;; Ensure cogutil is vendored
      (ensure-cogutil-vendored)
      
      ;; Analyze structure
      (let ((structure (analyze-cogutil-structure)))
        
        ;; Validate Guix environment
        (let ((guix-ok (validate-guix-environment)))
          
          (display "\n🎯 Cognitive Validation Results:\n")
          (if (and (file-exists? "cogutil/CMakeLists.txt") guix-ok)
              (begin
                (display "✅ ALL VALIDATIONS PASSED\n")
                (display "🧠 Cognitive system ready for Guix build\n")
                (exit 0))
              (begin
                (display "❌ SOME VALIDATIONS FAILED\n")
                (display "🔧 Please review the issues above\n")
                (exit 1))))))
    
    (lambda (key . args)
      (format #t "❌ Error during validation: ~a ~a\n" key args)
      (exit 1))))

;; Entry point
(when (batch-mode?)
  (main (command-line)))

;; Interactive mode help
(unless (batch-mode?)
  (display "🧠 Cognitive Vendoring Validation (Interactive Mode)\n")
  (display "Run (main '()) to start validation\n")
  (display "Or use: guile -s validate-cogutil-scheme.scm\n"))