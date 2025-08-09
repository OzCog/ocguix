#!/usr/bin/env guile
!#

;; Agent Management Controller for OpenCog Cognitive Ecosystem
;; Extends hypergraph schema with agent management nodes and operations
;; Integrates with SKZ autonomous agents framework

(use-modules 
  (srfi srfi-1)
  (srfi srfi-19)
  (ice-9 match)
  (ice-9 format)
  (ice-9 ports)
  (ice-9 textual-ports)
  (ice-9 threads)
  (ice-9 atomic)
  (ice-9 rdelim)
  (web client)
  (web response)
  (json))

;; Agent management configuration
(define agent-management-config
  '((api-server-port . 5002)
    (dashboard-port . 5002) 
    (refresh-interval . 5)
    (log-retention-days . 7)
    (auto-restart-enabled . #t)
    (max-restart-attempts . 3)))

;; Agent registry with hypergraph representation
(define cognitive-agent-registry (make-atomic-box '()))

;; Define agent management node types in hypergraph
(define (create-agent-management-node agent-id agent-type status capabilities)
  "Create an agent management node in the cognitive hypergraph"
  (let ((node-data
         `((agent-id . ,agent-id)
           (agent-type . ,agent-type) 
           (status . ,status)
           (capabilities . ,capabilities)
           (created-time . ,(current-time))
           (tensor-shape . (3 5 7))  ; Cognitive tensor dimensions
           (management-version . "1.0.0"))))
    
    (format #t "🧠 Creating agent management node: ~a~%" agent-id)
    
    ;; Store in agent registry
    (atomic-box-swap! cognitive-agent-registry
                      (lambda (registry)
                        (assoc-set! registry agent-id node-data)))
    
    node-data))

;; Agent lifecycle management functions
(define (register-cognitive-agent agent-id config)
  "Register a new cognitive agent in the management system"
  (format #t "📋 Registering cognitive agent: ~a~%" agent-id)
  
  (let ((agent-node (create-agent-management-node 
                     agent-id
                     (assoc-ref config 'type)
                     'registered
                     (assoc-ref config 'capabilities))))
    
    ;; Create hypergraph links
    (create-agent-capability-links agent-id (assoc-ref config 'capabilities))
    (create-agent-dependency-links agent-id (assoc-ref config 'dependencies))
    
    (format #t "✅ Agent ~a registered successfully~%" agent-id)
    agent-node))

(define (create-agent-capability-links agent-id capabilities)
  "Create hypergraph links for agent capabilities"
  (for-each
   (lambda (capability)
     (format #t "🔗 Linking capability: ~a -> ~a~%" agent-id capability)
     ;; In a real AtomSpace implementation, this would create actual links
     ;; For now, we simulate the hypergraph structure
     (let ((link-data
            `((type . capability-link)
              (agent . ,agent-id)
              (capability . ,capability)
              (strength . 0.9)
              (confidence . 0.8))))
       #t)) ; Placeholder
   capabilities))

(define (create-agent-dependency-links agent-id dependencies) 
  "Create hypergraph links for agent dependencies"
  (for-each
   (lambda (dependency)
     (format #t "🔗 Linking dependency: ~a -> ~a~%" agent-id dependency)
     (let ((link-data
            `((type . dependency-link)
              (dependent . ,agent-id)
              (dependency . ,dependency)
              (relationship . requires))))
       #t)) ; Placeholder
   dependencies))

;; Agent status monitoring
(define (get-agent-status agent-id)
  "Get current status of a cognitive agent"
  (let ((registry (atomic-box-ref cognitive-agent-registry)))
    (assoc-ref registry agent-id)))

(define (update-agent-status agent-id new-status)
  "Update agent status in the cognitive hypergraph"
  (format #t "📊 Updating agent status: ~a -> ~a~%" agent-id new-status)
  
  (atomic-box-swap! cognitive-agent-registry
                    (lambda (registry)
                      (let ((agent-data (assoc-ref registry agent-id)))
                        (if agent-data
                            (assoc-set! registry agent-id
                                       (assoc-set! agent-data 'status new-status))
                            registry))))
  
  ;; Create status change event in hypergraph
  (create-status-change-event agent-id new-status))

(define (create-status-change-event agent-id new-status)
  "Create a status change event node in the hypergraph"
  (let ((event-data
         `((type . status-change-event)
           (agent . ,agent-id)
           (new-status . ,new-status)
           (timestamp . ,(current-time))
           (event-id . ,(string-append "status-" (symbol->string agent-id) "-" 
                                      (number->string (current-time)))))))
    
    (format #t "📅 Status change event created: ~a~%" (assoc-ref event-data 'event-id))
    event-data))

;; Agent management HTTP API integration
(define (call-management-api endpoint method . data)
  "Call the agent management API server"
  (let ((url (string-append "http://localhost:5002/api" endpoint)))
    (format #t "🌐 API call: ~a ~a~%" method url)
    
    (catch #t
      (lambda ()
        (case method
          ((GET) 
           (call-with-values (lambda () (http-get url))
             (lambda (response body)
               (if (= (response-code response) 200)
                   (json-string->scm (utf8->string body))
                   (begin
                     (format #t "❌ API call failed: ~a~%" (response-code response))
                     #f)))))
          ((POST)
           (call-with-values 
               (lambda () 
                 (http-post url 
                           #:body (scm->json-string (if (null? data) '() (car data)))
                           #:headers '((content-type . (application/json)))))
             (lambda (response body)
               (if (= (response-code response) 200)
                   (json-string->scm (utf8->string body))
                   (begin
                     (format #t "❌ API call failed: ~a~%" (response-code response))
                     #f)))))))
      (lambda (key . args)
        (format #t "❌ API call exception: ~a ~a~%" key args)
        #f))))

;; High-level agent management operations
(define (start-cognitive-agent agent-id)
  "Start a cognitive agent via the management API"
  (format #t "▶️ Starting cognitive agent: ~a~%" agent-id)
  
  (update-agent-status agent-id 'starting)
  (let ((result (call-management-api (string-append "/agents/" (symbol->string agent-id) "/start") 'POST)))
    (if result
        (begin
          (update-agent-status agent-id 'running)
          (format #t "✅ Agent ~a started successfully~%" agent-id)
          #t)
        (begin
          (update-agent-status agent-id 'error)
          (format #t "❌ Failed to start agent ~a~%" agent-id)
          #f))))

(define (stop-cognitive-agent agent-id)
  "Stop a cognitive agent via the management API"
  (format #t "⏹️ Stopping cognitive agent: ~a~%" agent-id)
  
  (update-agent-status agent-id 'stopping)
  (let ((result (call-management-api (string-append "/agents/" (symbol->string agent-id) "/stop") 'POST)))
    (if result
        (begin
          (update-agent-status agent-id 'stopped)
          (format #t "✅ Agent ~a stopped successfully~%" agent-id)
          #t)
        (begin
          (update-agent-status agent-id 'error) 
          (format #t "❌ Failed to stop agent ~a~%" agent-id)
          #f))))

(define (restart-cognitive-agent agent-id)
  "Restart a cognitive agent"
  (format #t "🔄 Restarting cognitive agent: ~a~%" agent-id)
  
  (and (stop-cognitive-agent agent-id)
       (begin
         (sleep 1) ; Brief pause
         (start-cognitive-agent agent-id))))

;; Batch operations
(define (start-all-cognitive-agents)
  "Start all registered cognitive agents"
  (format #t "🚀 Starting all cognitive agents~%")
  
  (let ((result (call-management-api "/operations/start-all" 'POST)))
    (if result
        (begin
          (format #t "✅ All agents start operation completed~%")
          #t)
        (begin
          (format #t "❌ Failed to start all agents~%")
          #f))))

(define (stop-all-cognitive-agents)
  "Stop all cognitive agents"
  (format #t "🛑 Stopping all cognitive agents~%")
  
  (let ((result (call-management-api "/operations/stop-all" 'POST)))
    (if result
        (begin
          (format #t "✅ All agents stop operation completed~%")
          #t)
        (begin
          (format #t "❌ Failed to stop all agents~%")
          #f))))

;; Monitoring and diagnostics
(define (get-system-status)
  "Get overall system status from management API"
  (format #t "📊 Fetching system status~%")
  
  (let ((result (call-management-api "/system/status" 'GET)))
    (if result
        (begin
          (format #t "📈 System Status:~%")
          (format #t "  Agents: ~a running / ~a total~%" 
                  (assoc-ref (assoc-ref result 'agents) 'running)
                  (assoc-ref (assoc-ref result 'agents) 'total))
          (format #t "  CPU: ~a%~%" 
                  (assoc-ref (assoc-ref result 'system) 'cpu_percent))
          (format #t "  Memory: ~a%~%" 
                  (assoc-ref (assoc-ref result 'system) 'memory_percent))
          result)
        (begin
          (format #t "❌ Failed to get system status~%")
          #f))))

(define (get-agent-logs agent-id lines)
  "Get recent logs for a specific agent"
  (format #t "📄 Fetching logs for agent: ~a~%" agent-id)
  
  (let ((result (call-management-api 
                 (string-append "/agents/" (symbol->string agent-id) "/logs?lines=" (number->string lines))
                 'GET)))
    (if result
        (let ((logs (assoc-ref result 'logs)))
          (if (list? logs)
              (begin
                (format #t "📋 Recent logs for ~a:~%" agent-id)
                (for-each (lambda (log-line)
                            (format #t "  ~a~%" log-line))
                          logs)
                logs)
              (begin
                (format #t "📭 No logs available for ~a~%" agent-id)
                '())))
        (begin
          (format #t "❌ Failed to get logs for ~a~%" agent-id)
          #f))))

;; Hypergraph queries for agent management
(define (query-agents-by-capability capability)
  "Find all agents with a specific capability"
  (format #t "🔍 Querying agents by capability: ~a~%" capability)
  
  (let ((registry (atomic-box-ref cognitive-agent-registry)))
    (filter
     (lambda (agent-entry)
       (let ((agent-data (cdr agent-entry)))
         (member capability (assoc-ref agent-data 'capabilities))))
     registry)))

(define (query-agents-by-status status)
  "Find all agents with a specific status"
  (format #t "🔍 Querying agents by status: ~a~%" status)
  
  (let ((registry (atomic-box-ref cognitive-agent-registry)))
    (filter
     (lambda (agent-entry)
       (let ((agent-data (cdr agent-entry)))
         (eq? (assoc-ref agent-data 'status) status)))
     registry)))

;; Agent health monitoring
(define (check-agent-health agent-id)
  "Perform health check on a cognitive agent"
  (format #t "🏥 Checking health of agent: ~a~%" agent-id)
  
  (let ((agent-data (get-agent-status agent-id)))
    (if agent-data
        (let* ((status (assoc-ref agent-data 'status))
               (created-time (assoc-ref agent-data 'created-time))
               (uptime (- (current-time) created-time))
               (health-score (cond
                             ((eq? status 'running) 1.0)
                             ((eq? status 'starting) 0.7)
                             ((eq? status 'stopping) 0.5)
                             ((eq? status 'error) 0.1)
                             (else 0.0))))
          
          (format #t "💚 Health check for ~a: score=~a, uptime=~as~%" 
                  agent-id health-score uptime)
          
          `((agent . ,agent-id)
            (health-score . ,health-score)
            (uptime . ,uptime)
            (status . ,status)
            (timestamp . ,(current-time))))
        (begin
          (format #t "❌ Agent ~a not found in registry~%" agent-id)
          #f))))

;; Initialize default agents
(define (initialize-agent-management)
  "Initialize the agent management system with default agents"
  (format #t "🚀 Initializing Agent Management System~%")
  
  ;; Register SKZ agents
  (register-cognitive-agent 'skz-research-discovery
                           '((type . cognitive-agent)
                             (capabilities . (inci-database-mining patent-analysis trend-identification))
                             (dependencies . ())))
  
  (register-cognitive-agent 'skz-submission-assistant
                           '((type . cognitive-agent)
                             (capabilities . (quality-assessment safety-compliance statistical-review))
                             (dependencies . (skz-research-discovery))))
  
  (register-cognitive-agent 'skz-editorial-orchestration
                           '((type . orchestrator-agent)
                             (capabilities . (workflow-coordination decision-making conflict-resolution))
                             (dependencies . (skz-research-discovery skz-submission-assistant))))
  
  (register-cognitive-agent 'skz-atomspace-bridge
                           '((type . bridge-agent)
                             (capabilities . (atomspace-integration knowledge-representation))
                             (dependencies . ())))
  
  (format #t "✅ Agent Management System initialized~%")
  #t)

;; Export hypergraph data for external tools
(define (export-agent-hypergraph)
  "Export agent management hypergraph data"
  (let ((registry (atomic-box-ref cognitive-agent-registry)))
    (format #t "📤 Exporting agent hypergraph data~%")
    
    (let ((export-data
           `((type . agent-management-hypergraph)
             (version . "1.0.0")
             (timestamp . ,(current-time))
             (agents . ,registry)
             (tensor-dimensions . ((cognitive . 3) (capabilities . 5) (dependencies . 7))))))
      
      ;; Write to file
      (call-with-output-file "/tmp/agent-hypergraph-export.json"
        (lambda (port)
          (write (scm->json-string export-data) port)))
      
      (format #t "✅ Hypergraph data exported to /tmp/agent-hypergraph-export.json~%")
      export-data)))

;; Main command-line interface
(define (agent-management-cli args)
  "Command-line interface for agent management"
  (match args
    (("--init")
     (initialize-agent-management))
    
    (("--status")
     (get-system-status))
    
    (("--start" agent-id)
     (start-cognitive-agent (string->symbol agent-id)))
    
    (("--stop" agent-id)
     (stop-cognitive-agent (string->symbol agent-id)))
    
    (("--restart" agent-id)
     (restart-cognitive-agent (string->symbol agent-id)))
    
    (("--start-all")
     (start-all-cognitive-agents))
    
    (("--stop-all")
     (stop-all-cognitive-agents))
    
    (("--logs" agent-id . lines)
     (let ((num-lines (if (null? lines) 50 (string->number (car lines)))))
       (get-agent-logs (string->symbol agent-id) num-lines)))
    
    (("--health" agent-id)
     (check-agent-health (string->symbol agent-id)))
    
    (("--query-capability" capability)
     (query-agents-by-capability (string->symbol capability)))
    
    (("--query-status" status) 
     (query-agents-by-status (string->symbol status)))
    
    (("--export")
     (export-agent-hypergraph))
    
    (("--help")
     (format #t "Agent Management Controller - OpenCog Cognitive Ecosystem~%")
     (format #t "Usage: guile agent-management-controller.scm [OPTIONS]~%")
     (format #t "~%")
     (format #t "Options:~%")
     (format #t "  --init                    Initialize agent management system~%")
     (format #t "  --status                  Get overall system status~%")
     (format #t "  --start <agent-id>        Start specific agent~%")
     (format #t "  --stop <agent-id>         Stop specific agent~%") 
     (format #t "  --restart <agent-id>      Restart specific agent~%")
     (format #t "  --start-all               Start all agents~%")
     (format #t "  --stop-all                Stop all agents~%")
     (format #t "  --logs <agent-id> [lines] Get agent logs~%")
     (format #t "  --health <agent-id>       Check agent health~%")
     (format #t "  --query-capability <cap>  Query agents by capability~%")
     (format #t "  --query-status <status>   Query agents by status~%")
     (format #t "  --export                  Export hypergraph data~%")
     (format #t "  --help                    Show this help~%"))
    
    (_
     (format #t "❌ Invalid arguments. Use --help for usage information.~%")
     (exit 1))))

;; Main execution
(when (batch-mode?)
  (let ((args (cdr (command-line))))
    (if (null? args)
        (agent-management-cli '("--help"))
        (agent-management-cli args))))

;; Interactive REPL functions when loaded as module
(format #t "🧠 Agent Management Controller loaded~%")
(format #t "Available functions:~%")
(format #t "  (initialize-agent-management)~%")
(format #t "  (start-cognitive-agent 'agent-id)~%")
(format #t "  (stop-cognitive-agent 'agent-id)~%")
(format #t "  (get-system-status)~%")
(format #t "Use (agent-management-cli '(\"--help\")) for full CLI help~%")