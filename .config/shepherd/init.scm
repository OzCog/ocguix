;; Shepherd configuration for OpenCog development
;; This file defines services for building, testing, and running OpenCog packages

(use-modules (shepherd service)
             (shepherd service file-system)
             (ice-9 match))

;; OpenCog build service
(define opencog-build-service
  (service
   '(opencog-build)
   #:docstring "Build OpenCog packages using Guix"
   #:start (make-forkexec-constructor
            '("/bin/bash" "-c" "cd /workspace && guix build -f opencog.scm")
            #:user "vscode"
            #:group "vscode"
            #:directory "/workspace"
            #:log-file "/tmp/opencog-build.log")
   #:stop (make-kill-destructor)
   #:respawn? #f))

;; OpenCog test service  
(define opencog-test-service
  (service
   '(opencog-test)
   #:docstring "Run OpenCog tests"
   #:start (make-forkexec-constructor
            '("/bin/bash" "-c" "cd /workspace && guix build -f opencog.scm --check")
            #:user "vscode"
            #:group "vscode"
            #:directory "/workspace"
            #:log-file "/tmp/opencog-test.log")
   #:stop (make-kill-destructor)
   #:respawn? #f))

;; Cogutil build service
(define cogutil-build-service
  (service
   '(cogutil-build)
   #:docstring "Build cogutil package using Guix"
   #:start (make-forkexec-constructor
            '("/bin/bash" "-c" "cd /workspace && guix build -f guix.scm")
            #:user "vscode"
            #:group "vscode"
            #:directory "/workspace"
            #:log-file "/tmp/cogutil-build.log")
   #:stop (make-kill-destructor)
   #:respawn? #f))

;; Development environment service
(define dev-environment-service
  (service
   '(dev-environment)
   #:docstring "Set up development environment with Guix shell"
   #:start (make-forkexec-constructor
            '("/bin/bash" "-c" "cd /workspace && guix shell -f opencog.scm --pure")
            #:user "vscode"
            #:group "vscode"
            #:directory "/workspace"
            #:log-file "/tmp/dev-environment.log")
   #:stop (make-kill-destructor)
   #:respawn? #f))

;; Register all services
(register-services opencog-build-service
                   opencog-test-service
                   cogutil-build-service
                   dev-environment-service)

;; Start essential services automatically
(start 'cogutil-build)