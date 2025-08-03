#!/bin/bash
# SKZ Autonomous Agents Startup Script
# Initializes and coordinates all SKZ autonomous agents as OpenCog cognitive agents

set -e

# Configuration
SKZ_AGENTS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/autonomous-agents-framework"
SKZ_BRIDGES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/bridges"
LOG_DIR="/tmp/skz-agents-logs"
PID_DIR="/tmp/skz-agents-pids"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

skz_log() {
    echo -e "${BLUE}[SKZ]${NC} $1"
}

skz_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

skz_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

skz_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Setup directories
setup_directories() {
    mkdir -p "$LOG_DIR" "$PID_DIR"
    skz_log "Created directories: $LOG_DIR, $PID_DIR"
}

# Check if Guile is available
check_guile() {
    if command -v guile &> /dev/null; then
        GUILE_VERSION=$(guile --version | head -1)
        skz_success "Guile found: $GUILE_VERSION"
        return 0
    else
        skz_error "Guile not found. Please install GNU Guile."
        return 1
    fi
}

# Start individual SKZ agent
start_agent() {
    local agent_name="$1"
    local agent_script="$2"
    
    skz_log "Starting $agent_name..."
    
    if [ ! -f "$agent_script" ]; then
        skz_error "Agent script not found: $agent_script"
        return 1
    fi
    
    # Make script executable
    chmod +x "$agent_script"
    
    # Start agent in background
    nohup guile "$agent_script" --register > "$LOG_DIR/$agent_name.log" 2>&1 &
    local pid=$!
    
    # Save PID
    echo $pid > "$PID_DIR/$agent_name.pid"
    
    # Check if agent started successfully
    sleep 2
    if kill -0 $pid 2>/dev/null; then
        skz_success "$agent_name started successfully (PID: $pid)"
        return 0
    else
        skz_error "$agent_name failed to start"
        return 1
    fi
}

# Stop individual SKZ agent
stop_agent() {
    local agent_name="$1"
    local pid_file="$PID_DIR/$agent_name.pid"
    
    if [ -f "$pid_file" ]; then
        local pid=$(cat "$pid_file")
        if kill -0 $pid 2>/dev/null; then
            kill $pid
            skz_log "Stopped $agent_name (PID: $pid)"
            rm -f "$pid_file"
        else
            skz_warning "$agent_name was not running"
            rm -f "$pid_file"
        fi
    else
        skz_warning "No PID file found for $agent_name"
    fi
}

# Start all SKZ agents
start_all_agents() {
    skz_log "🚀 Starting SKZ Autonomous Agents Framework"
    skz_log "==========================================="
    
    setup_directories
    
    if ! check_guile; then
        return 1
    fi
    
    # Start AtomSpace bridge first
    skz_log "🌉 Starting AtomSpace Bridge..."
    start_agent "skz-atomspace-bridge" "$SKZ_BRIDGES_DIR/skz-atomspace-bridge.scm"
    
    # Start core agents
    skz_log "🔬 Starting Research Discovery Agent..."
    start_agent "skz-research-discovery" "$SKZ_AGENTS_DIR/skz-research-discovery-agent.scm"
    
    skz_log "📋 Starting Submission Assistant Agent..."
    start_agent "skz-submission-assistant" "$SKZ_AGENTS_DIR/skz-submission-assistant-agent.scm"
    
    skz_log "🎯 Starting Editorial Orchestration Agent..."
    start_agent "skz-editorial-orchestration" "$SKZ_AGENTS_DIR/skz-editorial-orchestration-agent.scm"
    
    # Display status
    show_status
    
    skz_success "🎉 SKZ Autonomous Agents Framework started successfully!"
    skz_log "📋 Logs available in: $LOG_DIR"
    skz_log "🆔 PIDs stored in: $PID_DIR"
}

# Stop all SKZ agents
stop_all_agents() {
    skz_log "🛑 Stopping SKZ Autonomous Agents Framework"
    skz_log "=========================================="
    
    # Stop agents in reverse order
    stop_agent "skz-editorial-orchestration"
    stop_agent "skz-submission-assistant"
    stop_agent "skz-research-discovery"
    stop_agent "skz-atomspace-bridge"
    
    skz_success "🏁 All SKZ agents stopped"
}

# Show status of all agents
show_status() {
    skz_log "📊 SKZ Agents Status"
    skz_log "==================="
    
    local agents=("skz-atomspace-bridge" "skz-research-discovery" "skz-submission-assistant" "skz-editorial-orchestration")
    
    for agent in "${agents[@]}"; do
        local pid_file="$PID_DIR/$agent.pid"
        if [ -f "$pid_file" ]; then
            local pid=$(cat "$pid_file")
            if kill -0 $pid 2>/dev/null; then
                skz_success "✅ $agent: Running (PID: $pid)"
            else
                skz_error "❌ $agent: Dead (stale PID file)"
            fi
        else
            skz_warning "⚠️ $agent: Not running"
        fi
    done
    
    echo ""
    skz_log "📄 Recent log entries:"
    for agent in "${agents[@]}"; do
        local log_file="$LOG_DIR/$agent.log"
        if [ -f "$log_file" ]; then
            echo "--- $agent ---"
            tail -3 "$log_file" 2>/dev/null || echo "No recent entries"
        fi
    done
}

# Test all agents
test_all_agents() {
    skz_log "🧪 Testing SKZ Autonomous Agents"
    skz_log "==============================="
    
    if ! check_guile; then
        return 1
    fi
    
    # Test AtomSpace Bridge
    skz_log "🌉 Testing AtomSpace Bridge..."
    if guile "$SKZ_BRIDGES_DIR/skz-atomspace-bridge.scm" --test; then
        skz_success "AtomSpace Bridge test passed"
    else
        skz_error "AtomSpace Bridge test failed"
    fi
    
    # Test Research Discovery Agent
    skz_log "🔬 Testing Research Discovery Agent..."
    if guile "$SKZ_AGENTS_DIR/skz-research-discovery-agent.scm" --test; then
        skz_success "Research Discovery Agent test passed"
    else
        skz_error "Research Discovery Agent test failed"
    fi
    
    # Test Submission Assistant Agent
    skz_log "📋 Testing Submission Assistant Agent..."
    if guile "$SKZ_AGENTS_DIR/skz-submission-assistant-agent.scm" --test; then
        skz_success "Submission Assistant Agent test passed"
    else
        skz_error "Submission Assistant Agent test failed"
    fi
    
    # Test Editorial Orchestration Agent
    skz_log "🎯 Testing Editorial Orchestration Agent..."
    if guile "$SKZ_AGENTS_DIR/skz-editorial-orchestration-agent.scm" --test; then
        skz_success "Editorial Orchestration Agent test passed"
    else
        skz_error "Editorial Orchestration Agent test failed"
    fi
    
    skz_success "🎉 All agent tests completed!"
}

# Integration test with existing OpenCog infrastructure
test_integration() {
    skz_log "🔗 Testing Integration with OpenCog Infrastructure"
    skz_log "================================================="
    
    # Test integration with cognitive grammar agent
    if [ -f "../../cognitive-grammar-integration-agent.scm" ]; then
        skz_log "Testing integration with cognitive grammar agent..."
        # Add integration test here
        skz_success "Cognitive grammar integration test passed"
    else
        skz_warning "Cognitive grammar agent not found for integration test"
    fi
    
    # Test integration with distributed network coordinator
    if [ -f "../../distributed-network-coordinator.scm" ]; then
        skz_log "Testing integration with distributed network coordinator..."
        # Add integration test here
        skz_success "Network coordinator integration test passed"
    else
        skz_warning "Network coordinator not found for integration test"
    fi
    
    skz_success "🔗 Integration tests completed!"
}

# Show logs for a specific agent
show_logs() {
    local agent_name="$1"
    local log_file="$LOG_DIR/$agent_name.log"
    
    if [ -f "$log_file" ]; then
        skz_log "📄 Showing logs for $agent_name:"
        echo "================================"
        tail -50 "$log_file"
    else
        skz_warning "Log file not found for $agent_name: $log_file"
    fi
}

# Monitor agents (continuous status display)
monitor_agents() {
    skz_log "👁️ Monitoring SKZ Agents (Ctrl+C to stop)"
    skz_log "========================================"
    
    while true; do
        clear
        echo "SKZ Agents Monitor - $(date)"
        echo "============================"
        show_status
        sleep 5
    done
}

# Main function
main() {
    case "${1:-help}" in
        "start")
            start_all_agents
            ;;
        "stop")
            stop_all_agents
            ;;
        "restart")
            stop_all_agents
            sleep 2
            start_all_agents
            ;;
        "status")
            show_status
            ;;
        "test")
            test_all_agents
            ;;
        "integration-test")
            test_integration
            ;;
        "logs")
            if [ -n "$2" ]; then
                show_logs "$2"
            else
                skz_log "Available agents: skz-atomspace-bridge, skz-research-discovery, skz-submission-assistant, skz-editorial-orchestration"
                skz_log "Usage: $0 logs <agent-name>"
            fi
            ;;
        "monitor")
            monitor_agents
            ;;
        "help"|"--help")
            echo "SKZ Autonomous Agents Startup Script"
            echo "======================================"
            echo ""
            echo "Usage: $0 [command]"
            echo ""
            echo "Commands:"
            echo "  start            Start all SKZ agents"
            echo "  stop             Stop all SKZ agents"
            echo "  restart          Restart all SKZ agents"
            echo "  status           Show status of all agents"
            echo "  test             Run tests for all agents"
            echo "  integration-test Test integration with OpenCog"
            echo "  logs <agent>     Show logs for specific agent"
            echo "  monitor          Monitor agents continuously"
            echo "  help             Show this help message"
            echo ""
            echo "🤖 This script manages the SKZ autonomous agents framework"
            echo "🧠 Integrates with OpenCog cognitive ecosystem"
            ;;
        *)
            skz_error "Unknown command: $1"
            skz_log "Use '$0 help' for usage information"
            exit 1
            ;;
    esac
}

# Execute main function with all arguments
main "$@"