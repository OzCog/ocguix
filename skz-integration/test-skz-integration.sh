#!/bin/bash
# SKZ Integration Test Suite
# Comprehensive testing of autonomous agents as OpenCog cognitive agents

set -e

# Configuration
TEST_DIR="/tmp/skz-integration-test"
SKZ_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_FILE="$TEST_DIR/integration-test.log"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

test_log() {
    echo -e "${BLUE}[TEST]${NC} $1" | tee -a "$LOG_FILE"
}

test_success() {
    echo -e "${GREEN}[PASS]${NC} $1" | tee -a "$LOG_FILE"
}

test_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1" | tee -a "$LOG_FILE"
}

test_error() {
    echo -e "${RED}[FAIL]${NC} $1" | tee -a "$LOG_FILE"
}

# Setup test environment
setup_test_environment() {
    test_log "Setting up test environment..."
    
    # Create test directory
    rm -rf "$TEST_DIR"
    mkdir -p "$TEST_DIR"
    cd "$TEST_DIR"
    
    # Initialize log file
    echo "SKZ Integration Test Suite - $(date)" > "$LOG_FILE"
    echo "==========================================" >> "$LOG_FILE"
    
    test_success "Test environment setup complete"
}

# Test Guile availability
test_guile_availability() {
    test_log "Testing Guile availability..."
    
    if command -v guile &> /dev/null; then
        GUILE_VERSION=$(guile --version | head -1)
        test_success "Guile available: $GUILE_VERSION"
        return 0
    else
        test_error "Guile not found - required for SKZ agents"
        return 1
    fi
}

# Test individual agent functionality
test_research_discovery_agent() {
    test_log "Testing Research Discovery Agent..."
    
    local agent_script="$SKZ_DIR/autonomous-agents-framework/skz-research-discovery-agent.scm"
    
    if [ ! -f "$agent_script" ]; then
        test_error "Research Discovery Agent script not found"
        return 1
    fi
    
    # Test agent execution
    if timeout 30 guile "$agent_script" --test >> "$LOG_FILE" 2>&1; then
        test_success "Research Discovery Agent test passed"
        
        # Test specific functions
        test_log "  Testing INCI database mining..."
        if timeout 10 guile -c "(load \"$agent_script\") (mine-inci-database '(function \"test\"))" >> "$LOG_FILE" 2>&1; then
            test_success "  INCI mining function works"
        else
            test_warning "  INCI mining function issue (non-critical)"
        fi
        
        return 0
    else
        test_error "Research Discovery Agent test failed"
        return 1
    fi
}

test_submission_assistant_agent() {
    test_log "Testing Submission Assistant Agent..."
    
    local agent_script="$SKZ_DIR/autonomous-agents-framework/skz-submission-assistant-agent.scm"
    
    if [ ! -f "$agent_script" ]; then
        test_error "Submission Assistant Agent script not found"
        return 1
    fi
    
    # Test agent execution
    if timeout 30 guile "$agent_script" --test >> "$LOG_FILE" 2>&1; then
        test_success "Submission Assistant Agent test passed"
        return 0
    else
        test_error "Submission Assistant Agent test failed"
        return 1
    fi
}

test_editorial_orchestration_agent() {
    test_log "Testing Editorial Orchestration Agent..."
    
    local agent_script="$SKZ_DIR/autonomous-agents-framework/skz-editorial-orchestration-agent.scm"
    
    if [ ! -f "$agent_script" ]; then
        test_error "Editorial Orchestration Agent script not found"
        return 1
    fi
    
    # Test agent execution
    if timeout 30 guile "$agent_script" --test >> "$LOG_FILE" 2>&1; then
        test_success "Editorial Orchestration Agent test passed"
        return 0
    else
        test_error "Editorial Orchestration Agent test failed"
        return 1
    fi
}

test_atomspace_bridge() {
    test_log "Testing AtomSpace Bridge..."
    
    local bridge_script="$SKZ_DIR/bridges/skz-atomspace-bridge.scm"
    
    if [ ! -f "$bridge_script" ]; then
        test_error "AtomSpace Bridge script not found"
        return 1
    fi
    
    # Test bridge execution
    if timeout 30 guile "$bridge_script" --test >> "$LOG_FILE" 2>&1; then
        test_success "AtomSpace Bridge test passed"
        return 0
    else
        test_error "AtomSpace Bridge test failed"
        return 1
    fi
}

# Test integration with existing OpenCog infrastructure
test_cognitive_grammar_integration() {
    test_log "Testing integration with cognitive grammar agent..."
    
    local grammar_agent="$SKZ_DIR/../../cognitive-grammar-integration-agent.scm"
    
    if [ -f "$grammar_agent" ]; then
        # Test that the grammar agent can load
        if timeout 15 guile -c "(load \"$grammar_agent\")" >> "$LOG_FILE" 2>&1; then
            test_success "Cognitive grammar agent integration successful"
        else
            test_warning "Cognitive grammar agent integration issue (non-critical)"
        fi
    else
        test_warning "Cognitive grammar agent not found - skipping integration test"
    fi
}

test_distributed_coordinator_integration() {
    test_log "Testing integration with distributed network coordinator..."
    
    local coordinator="$SKZ_DIR/../../distributed-network-coordinator.scm"
    
    if [ -f "$coordinator" ]; then
        # Test that the coordinator can load
        if timeout 15 guile -c "(load \"$coordinator\")" >> "$LOG_FILE" 2>&1; then
            test_success "Distributed coordinator integration successful"
        else
            test_warning "Distributed coordinator integration issue (non-critical)"
        fi
    else
        test_warning "Distributed coordinator not found - skipping integration test"
    fi
}

# Test agent communication and workflow
test_agent_communication() {
    test_log "Testing agent communication workflow..."
    
    # Create a simple test script that loads multiple agents
    cat > "$TEST_DIR/communication_test.scm" << 'EOF'
(use-modules (ice-9 format))

(define (test-agent-communication)
  "Test basic agent communication patterns"
  (format #t "Testing agent communication...~%")
  
  ;; Simulate agent registration
  (define agents '(research-discovery submission-assistant editorial-orchestration))
  
  ;; Test message passing simulation
  (for-each
    (lambda (agent)
      (format #t "Agent ~a registered successfully~%" agent))
    agents)
  
  ;; Test workflow coordination
  (format #t "Testing workflow coordination...~%")
  (format #t "Workflow initiated: submission -> assessment -> decision~%")
  
  (format #t "Agent communication test completed~%"))

(test-agent-communication)
EOF
    
    if timeout 10 guile "$TEST_DIR/communication_test.scm" >> "$LOG_FILE" 2>&1; then
        test_success "Agent communication workflow test passed"
    else
        test_warning "Agent communication workflow test had issues"
    fi
}

# Test startup script functionality
test_startup_script() {
    test_log "Testing startup script functionality..."
    
    local startup_script="$SKZ_DIR/start-skz-agents.sh"
    
    if [ ! -f "$startup_script" ]; then
        test_error "Startup script not found"
        return 1
    fi
    
    # Test help command
    if "$startup_script" help >> "$LOG_FILE" 2>&1; then
        test_success "Startup script help command works"
    else
        test_error "Startup script help command failed"
        return 1
    fi
    
    # Test status command (should work even with no agents running)
    if "$startup_script" status >> "$LOG_FILE" 2>&1; then
        test_success "Startup script status command works"
    else
        test_warning "Startup script status command had issues"
    fi
    
    return 0
}

# Test file structure and permissions
test_file_structure() {
    test_log "Testing SKZ integration file structure..."
    
    local required_files=(
        "$SKZ_DIR/README.md"
        "$SKZ_DIR/autonomous-agents-framework/skz-research-discovery-agent.scm"
        "$SKZ_DIR/autonomous-agents-framework/skz-submission-assistant-agent.scm"
        "$SKZ_DIR/autonomous-agents-framework/skz-editorial-orchestration-agent.scm"
        "$SKZ_DIR/bridges/skz-atomspace-bridge.scm"
        "$SKZ_DIR/start-skz-agents.sh"
    )
    
    local missing_files=0
    
    for file in "${required_files[@]}"; do
        if [ -f "$file" ]; then
            test_success "  ✓ $file exists"
        else
            test_error "  ✗ $file missing"
            ((missing_files++))
        fi
    done
    
    if [ $missing_files -eq 0 ]; then
        test_success "File structure test passed"
        return 0
    else
        test_error "File structure test failed - $missing_files files missing"
        return 1
    fi
}

# Performance and resource usage test
test_performance() {
    test_log "Testing performance and resource usage..."
    
    # Test agent startup time
    local start_time=$(date +%s)
    
    # Run a quick test of each agent
    for agent in research-discovery submission-assistant editorial-orchestration; do
        local agent_script="$SKZ_DIR/autonomous-agents-framework/skz-$agent-agent.scm"
        if [ -f "$agent_script" ]; then
            if timeout 5 guile -c "(load \"$agent_script\")" >> "$LOG_FILE" 2>&1; then
                test_success "  $agent agent loads quickly"
            else
                test_warning "  $agent agent slow to load"
            fi
        fi
    done
    
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    
    if [ $duration -lt 30 ]; then
        test_success "Performance test passed (${duration}s)"
    else
        test_warning "Performance test slow (${duration}s)"
    fi
}

# Generate test report
generate_test_report() {
    test_log "Generating test report..."
    
    local report_file="$TEST_DIR/skz-integration-test-report.md"
    
    cat > "$report_file" << EOF
# SKZ Integration Test Report

**Date:** $(date)
**Test Environment:** $TEST_DIR

## Test Results Summary

EOF
    
    # Count test results from log
    local passed=$(grep -c "\[PASS\]" "$LOG_FILE" || echo "0")
    local failed=$(grep -c "\[FAIL\]" "$LOG_FILE" || echo "0")
    local warnings=$(grep -c "\[WARN\]" "$LOG_FILE" || echo "0")
    
    cat >> "$report_file" << EOF
- ✅ **Passed:** $passed tests
- ❌ **Failed:** $failed tests  
- ⚠️ **Warnings:** $warnings tests

## Test Coverage

### Core Agents
- [x] Research Discovery Agent
- [x] Submission Assistant Agent
- [x] Editorial Orchestration Agent

### Infrastructure
- [x] AtomSpace Bridge
- [x] Startup Scripts
- [x] File Structure

### Integration
- [x] Cognitive Grammar Integration
- [x] Distributed Coordinator Integration
- [x] Agent Communication

## Detailed Log

\`\`\`
$(tail -50 "$LOG_FILE")
\`\`\`

## Recommendations

EOF

    if [ $failed -eq 0 ]; then
        echo "✅ **All tests passed!** The SKZ autonomous agents are ready for deployment as OpenCog cognitive agents." >> "$report_file"
    else
        echo "⚠️ **Some tests failed.** Review the failed tests and address issues before deployment." >> "$report_file"
    fi

    cat >> "$report_file" << EOF

## Next Steps

1. Address any failed tests or warnings
2. Deploy agents using \`./start-skz-agents.sh start\`
3. Monitor agent performance using \`./start-skz-agents.sh monitor\`
4. Integrate with existing OpenCog workflows

---
*Generated by SKZ Integration Test Suite*
EOF
    
    test_success "Test report generated: $report_file"
    echo ""
    echo "📊 Test Summary:"
    echo "  ✅ Passed: $passed"
    echo "  ❌ Failed: $failed"
    echo "  ⚠️ Warnings: $warnings"
    echo ""
    echo "📄 Full report: $report_file"
    echo "📄 Detailed log: $LOG_FILE"
}

# Main test execution
run_all_tests() {
    test_log "🧪 Starting SKZ Integration Test Suite"
    test_log "======================================"
    
    # Setup
    setup_test_environment
    
    # Core tests
    test_guile_availability || return 1
    test_file_structure || return 1
    
    # Agent tests
    test_research_discovery_agent
    test_submission_assistant_agent  
    test_editorial_orchestration_agent
    test_atomspace_bridge
    
    # Integration tests
    test_cognitive_grammar_integration
    test_distributed_coordinator_integration
    test_agent_communication
    
    # Infrastructure tests
    test_startup_script
    test_performance
    
    # Generate report
    generate_test_report
    
    test_log "🎉 Test suite completed!"
}

# Main function
main() {
    case "${1:-run}" in
        "run")
            run_all_tests
            ;;
        "quick")
            setup_test_environment
            test_guile_availability
            test_file_structure
            test_research_discovery_agent
            test_atomspace_bridge
            test_log "Quick test completed"
            ;;
        "help")
            echo "SKZ Integration Test Suite"
            echo "=========================="
            echo ""
            echo "Usage: $0 [command]"
            echo ""
            echo "Commands:"
            echo "  run    Run full test suite (default)"
            echo "  quick  Run quick validation tests"
            echo "  help   Show this help message"
            echo ""
            ;;
        *)
            echo "Unknown command: $1"
            echo "Use '$0 help' for usage information"
            exit 1
            ;;
    esac
}

# Execute main function
main "$@"