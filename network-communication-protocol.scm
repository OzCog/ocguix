#!/usr/bin/env guile
!#

;; Network Communication Protocol for Distributed Cognitive Grammar
;; Implements inter-agent communication and message routing
;; Part of issue #77: "integrate the repo as a distributed network of agentic cognitive grammar"

(use-modules 
  (srfi srfi-1)
  (srfi srfi-19)
  (ice-9 match)
  (ice-9 format)
  (ice-9 ports)
  (ice-9 textual-ports)
  (ice-9 atomic))

;; Network message types and routing protocols
(define message-protocols
  '((direct-message
     (format . "(from-agent to-agent message-type payload)")
     (routing . "point-to-point")
     (delivery . "synchronous")
     (example . "(cognitive-grammar registry-discovery query-request \"opencog packages\")"))
    
    (broadcast-message
     (format . "(from-agent broadcast message-type payload)")
     (routing . "multicast")
     (delivery . "asynchronous")
     (example . "(meta-cognitive broadcast health-update (status . optimal))"))
    
    (pipeline-message
     (format . "(coordinator agent-sequence pipeline-step data)")
     (routing . "sequential")
     (delivery . "synchronous")
     (example . "(coordinator (registry profile artifact) step-1 discovery-results)"))
    
    (coordination-message
     (format . "(coordinator target-agents coordination-type parameters)")
     (routing . "multicast-coordination")
     (delivery . "synchronous")
     (example . "(coordinator all load-balance (threshold . 0.8))"))))

;; Message queue and routing tables
(define global-message-queue (make-atomic-box '()))
(define routing-table (make-atomic-box '()))
(define agent-registry (make-atomic-box '()))

;; Core communication functions
(define (register-agent-endpoint agent-id endpoint capabilities)
  "Register an agent endpoint in the network routing table"
  (let ((current-table (atomic-box-ref routing-table))
        (agent-info (list agent-id endpoint capabilities (current-time))))
    (atomic-box-set! routing-table 
      (cons agent-info current-table))
    (format #t "📡 Registered agent endpoint: ~a -> ~a~%" agent-id endpoint)
    #t))

(define (send-message from-agent to-agent message-type payload)
  "Send a message from one agent to another"
  (let ((message (list 'direct-message from-agent to-agent message-type payload (current-time))))
    (enqueue-message message)
    (route-message message)
    (format #t "📧 Message sent: ~a -> ~a (~a)~%" from-agent to-agent message-type)))

(define (broadcast-message from-agent message-type payload)
  "Broadcast a message to all registered agents"
  (let ((message (list 'broadcast-message from-agent 'all message-type payload (current-time))))
    (enqueue-message message)
    (route-broadcast message)
    (format #t "📢 Broadcast sent: ~a -> all (~a)~%" from-agent message-type)))

(define (send-pipeline-message coordinator agent-sequence step-number data)
  "Send a message through a pipeline of agents"
  (let ((message (list 'pipeline-message coordinator agent-sequence step-number data (current-time))))
    (enqueue-message message)
    (route-pipeline message)
    (format #t "⚙️ Pipeline message: ~a -> ~a (step ~a)~%" coordinator agent-sequence step-number)))

(define (send-coordination-message coordinator target-agents coordination-type parameters)
  "Send a coordination message to multiple agents"
  (let ((message (list 'coordination-message coordinator target-agents coordination-type parameters (current-time))))
    (enqueue-message message)
    (route-coordination message)
    (format #t "🤝 Coordination message: ~a -> ~a (~a)~%" coordinator target-agents coordination-type)))

;; Message routing functions
(define (route-message message)
  "Route a direct message to its destination"
  (match message
    (('direct-message from to msg-type payload timestamp)
     (let ((target-endpoint (find-agent-endpoint to)))
       (if target-endpoint
           (deliver-message target-endpoint message)
           (format #t "❌ Cannot route message: agent ~a not found~%" to))))
    (_ 
     (format #t "⚠️ Invalid message format for routing~%"))))

(define (route-broadcast message)
  "Route a broadcast message to all agents"
  (match message
    (('broadcast-message from target msg-type payload timestamp)
     (let ((all-agents (atomic-box-ref routing-table)))
       (for-each
         (lambda (agent-info)
           (let ((agent-id (car agent-info))
                 (endpoint (cadr agent-info)))
             (unless (eq? agent-id from)
               (deliver-message endpoint message))))
         all-agents)))
    (_ 
     (format #t "⚠️ Invalid broadcast message format~%"))))

(define (route-pipeline message)
  "Route a pipeline message through the sequence of agents"
  (match message
    (('pipeline-message coordinator sequence step-number data timestamp)
     (if (and (list? sequence) (> (length sequence) 0))
         (let ((current-agent (car sequence))
               (remaining-agents (cdr sequence)))
           (format #t "🔗 Pipeline step ~a: processing with ~a~%" step-number current-agent)
           (let ((endpoint (find-agent-endpoint current-agent)))
             (if endpoint
                 (begin
                   (deliver-message endpoint message)
                   (when (> (length remaining-agents) 0)
                     (send-pipeline-message coordinator remaining-agents (+ step-number 1) data)))
                 (format #t "❌ Pipeline broken: agent ~a not found~%" current-agent))))
         (format #t "✅ Pipeline completed~%")))
    (_ 
     (format #t "⚠️ Invalid pipeline message format~%"))))

(define (route-coordination message)
  "Route a coordination message to target agents"
  (match message
    (('coordination-message coordinator targets coord-type parameters timestamp)
     (let ((target-list (if (eq? targets 'all)
                            (map car (atomic-box-ref routing-table))
                            targets)))
       (for-each
         (lambda (agent-id)
           (let ((endpoint (find-agent-endpoint agent-id)))
             (if endpoint
                 (deliver-message endpoint message)
                 (format #t "❌ Coordination target not found: ~a~%" agent-id))))
         target-list)))
    (_ 
     (format #t "⚠️ Invalid coordination message format~%"))))

;; Message delivery and processing
(define (deliver-message endpoint message)
  "Deliver a message to an agent endpoint"
  (format #t "📦 Delivering message to endpoint: ~a~%" endpoint)
  (match endpoint
    ((string? endpoint-str)
     (if (string-prefix? "local://" endpoint-str)
         (deliver-local-message endpoint-str message)
         (deliver-remote-message endpoint-str message)))
    (_ 
     (format #t "⚠️ Invalid endpoint format: ~a~%" endpoint))))

(define (deliver-local-message endpoint message)
  "Deliver a message to a local agent"
  (let ((agent-script (string-drop endpoint 8))) ; Remove "local://" prefix
    (format #t "🏠 Local delivery to: ~a~%" agent-script)
    ;; In a real implementation, this would invoke the agent script
    ;; For now, we simulate the delivery
    (process-agent-message agent-script message)))

(define (deliver-remote-message endpoint message)
  "Deliver a message to a remote agent (future extension)"
  (format #t "🌐 Remote delivery to: ~a~%" endpoint)
  ;; Placeholder for future remote delivery implementation
  (format #t "   Remote delivery not yet implemented~%"))

(define (process-agent-message agent-script message)
  "Process a message for a specific agent"
  (format #t "🤖 Processing message for agent: ~a~%" agent-script)
  (match message
    (('direct-message from to msg-type payload timestamp)
     (format #t "   Direct message: ~a -> ~a (~a)~%" from to msg-type)
     (format #t "   Payload: ~a~%" payload))
    (('broadcast-message from target msg-type payload timestamp)
     (format #t "   Broadcast message: ~a (~a)~%" from msg-type)
     (format #t "   Payload: ~a~%" payload))
    (('pipeline-message coordinator sequence step data timestamp)
     (format #t "   Pipeline message: step ~a~%" step)
     (format #t "   Data: ~a~%" data))
    (('coordination-message coordinator targets coord-type params timestamp)
     (format #t "   Coordination message: ~a~%" coord-type)
     (format #t "   Parameters: ~a~%" params))
    (_ 
     (format #t "   Unknown message type~%"))))

;; Utility functions
(define (find-agent-endpoint agent-id)
  "Find the endpoint for a given agent ID"
  (let ((routing-entries (atomic-box-ref routing-table)))
    (let ((entry (find (lambda (e) (eq? (car e) agent-id)) routing-entries)))
      (if entry (cadr entry) #f))))

(define (enqueue-message message)
  "Add a message to the global message queue"
  (let ((current-queue (atomic-box-ref global-message-queue)))
    (atomic-box-set! global-message-queue (cons message current-queue))))

(define (get-message-queue)
  "Get all messages from the queue"
  (atomic-box-ref global-message-queue))

(define (clear-message-queue)
  "Clear the message queue"
  (atomic-box-set! global-message-queue '()))

;; Network status and monitoring
(define (get-network-status)
  "Get the current network status"
  (let ((agents (atomic-box-ref routing-table))
        (messages (atomic-box-ref global-message-queue)))
    (format #t "🌐 Network Status~%")
    (format #t "================~%")
    (format #t "📡 Registered agents: ~a~%" (length agents))
    (format #t "📧 Messages in queue: ~a~%" (length messages))
    (format #t "~%🤖 Active Agents:~%")
    (for-each
      (lambda (agent)
        (format #t "   • ~a -> ~a~%" (car agent) (cadr agent)))
      agents)
    (when (> (length messages) 0)
      (format #t "~%📬 Recent Messages:~%")
      (for-each
        (lambda (msg)
          (format #t "   • ~a~%" (car msg)))
        (take messages (min 5 (length messages)))))))

;; Test and demonstration functions
(define (initialize-test-network)
  "Initialize a test network with sample agents"
  (format #t "🔧 Initializing test network...~%")
  
  ;; Register test agents
  (register-agent-endpoint 'cognitive-grammar "local://cognitive-grammar-integration-agent.scm" 
                          '("pattern-routing" "bridge-coordination"))
  (register-agent-endpoint 'registry-discovery "local://registry-discovery-agent.scm" 
                          '("package-enumeration" "metadata-extraction"))
  (register-agent-endpoint 'profile-extraction "local://profile-extraction-agent.scm" 
                          '("profile-analysis" "build-configuration"))
  (register-agent-endpoint 'artifact-synthesis "local://artifact-synthesis-agent.scm" 
                          '("manifest-generation" "dockerfile-creation"))
  (register-agent-endpoint 'meta-cognitive "local://meta-cognitive-feedback-agent.scm" 
                          '("health-monitoring" "improvement-analysis"))
  
  (format #t "✅ Test network initialized~%"))

(define (demonstrate-communication)
  "Demonstrate various communication patterns"
  (format #t "~%🎯 Demonstrating Communication Patterns~%")
  (format #t "======================================~%")
  
  ;; Direct message
  (format #t "~%1. Direct Message:~%")
  (send-message 'cognitive-grammar 'registry-discovery 'query "search opencog packages")
  
  ;; Broadcast message
  (format #t "~%2. Broadcast Message:~%")
  (broadcast-message 'meta-cognitive 'health-update '(status . optimal))
  
  ;; Pipeline message
  (format #t "~%3. Pipeline Message:~%")
  (send-pipeline-message 'coordinator 
                         '(registry-discovery profile-extraction artifact-synthesis) 
                         1 
                         '(discovery-request . "opencog"))
  
  ;; Coordination message
  (format #t "~%4. Coordination Message:~%")
  (send-coordination-message 'coordinator 
                            '(cognitive-grammar registry-discovery) 
                            'load-balance 
                            '(threshold . 0.8)))

(define (test-network-protocol)
  "Test the complete network communication protocol"
  (format #t "🧪 Testing Network Communication Protocol~%")
  (format #t "========================================~%")
  
  (initialize-test-network)
  (demonstrate-communication)
  (get-network-status)
  
  (format #t "~%✅ Network protocol test completed~%"))

;; Main entry point
(define (main args)
  "Main entry point for the network communication protocol"
  (cond
    ((null? (cdr args))
     (format #t "Network Communication Protocol for Distributed Cognitive Grammar~%")
     (format #t "==============================================================~%")
     (format #t "Usage: ~a <command>~%" (car args))
     (format #t "Commands:~%")
     (format #t "  --test       Run network communication tests~%")
     (format #t "  --status     Show network status~%")
     (format #t "  --demo       Demonstrate communication patterns~%")
     (format #t "  --init       Initialize test network~%"))
    
    ((string=? (cadr args) "--test")
     (test-network-protocol))
    
    ((string=? (cadr args) "--status")
     (get-network-status))
    
    ((string=? (cadr args) "--demo")
     (initialize-test-network)
     (demonstrate-communication))
    
    ((string=? (cadr args) "--init")
     (initialize-test-network))
    
    (else
     (format #t "Unknown command: ~a~%" (cadr args)))))

;; If running as script
(when (batch-mode?)
  (main (command-line)))

;; Export key functions for use by other modules
(define (protocol-send-message from to type payload) (send-message from to type payload))
(define (protocol-broadcast from type payload) (broadcast-message from type payload))
(define (protocol-register-agent id endpoint caps) (register-agent-endpoint id endpoint caps))
(define (protocol-get-status) (get-network-status))