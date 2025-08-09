#!/bin/bash
# Agent Management System Startup Script
# Initializes the complete agent management control system for OpenCog cognitive ecosystem

set -e

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
API_PORT=5002
DASHBOARD_URL="http://localhost:${API_PORT}"
LOG_DIR="/tmp/agent-management-logs"
PID_DIR="/tmp/agent-management-pids"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
PURPLE='\033[0;35m'
NC='\033[0m' # No Color

agent_mgmt_log() {
    echo -e "${PURPLE}[AGENT-MGMT]${NC} $1"
}

agent_mgmt_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

agent_mgmt_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

agent_mgmt_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Setup directories
setup_directories() {
    mkdir -p "$LOG_DIR" "$PID_DIR"
    agent_mgmt_log "Created directories: $LOG_DIR, $PID_DIR"
}

# Check dependencies
check_dependencies() {
    agent_mgmt_log "Checking dependencies..."
    
    local missing_deps=()
    
    # Check Python
    if ! command -v python3 &> /dev/null; then
        missing_deps+=("python3")
    fi
    
    # Check Guile
    if ! command -v guile &> /dev/null; then
        missing_deps+=("guile")
    fi
    
    # Check required Python packages
    if ! python3 -c "import flask, psutil" &> /dev/null; then
        agent_mgmt_warning "Python dependencies missing. Installing..."
        pip3 install flask flask-cors psutil &> /dev/null || {
            missing_deps+=("python packages (flask, flask-cors, psutil)")
        }
    fi
    
    if [ ${#missing_deps[@]} -ne 0 ]; then
        agent_mgmt_error "Missing dependencies: ${missing_deps[*]}"
        return 1
    fi
    
    agent_mgmt_success "All dependencies satisfied"
    return 0
}

# Initialize agent management system
initialize_system() {
    agent_mgmt_log "🚀 Initializing Agent Management System"
    
    # Initialize Scheme-based agent management controller
    agent_mgmt_log "Initializing agent management controller..."
    if guile "$SCRIPT_DIR/agent-management-controller.scm" --init > "$LOG_DIR/controller-init.log" 2>&1; then
        agent_mgmt_success "Agent management controller initialized"
    else
        agent_mgmt_warning "Agent management controller initialization had warnings (check logs)"
    fi
    
    # Initialize AtomSpace bridge
    agent_mgmt_log "Initializing AtomSpace bridge..."
    if guile "$SCRIPT_DIR/skz-integration/bridges/atomspace-bridge.scm" --test > "$LOG_DIR/bridge-init.log" 2>&1; then
        agent_mgmt_success "AtomSpace bridge initialized"
    else
        agent_mgmt_warning "AtomSpace bridge initialization had warnings (check logs)"
    fi
}

# Start the API server
start_api_server() {
    agent_mgmt_log "🌐 Starting Agent Management API Server..."
    
    # Check if already running
    if [ -f "$PID_DIR/api-server.pid" ]; then
        local pid=$(cat "$PID_DIR/api-server.pid")
        if kill -0 $pid 2>/dev/null; then
            agent_mgmt_warning "API server already running (PID: $pid)"
            return 0
        else
            rm -f "$PID_DIR/api-server.pid"
        fi
    fi
    
    # Start API server in background
    nohup python3 "$SCRIPT_DIR/agent-management-api.py" > "$LOG_DIR/api-server.log" 2>&1 &
    local api_pid=$!
    
    # Save PID
    echo $api_pid > "$PID_DIR/api-server.pid"
    
    # Wait for server to start
    agent_mgmt_log "Waiting for API server to start..."
    local max_attempts=10
    local attempt=1
    
    while [ $attempt -le $max_attempts ]; do
        if curl -s "http://localhost:${API_PORT}/api/system/status" > /dev/null 2>&1; then
            agent_mgmt_success "API server started successfully (PID: $api_pid)"
            agent_mgmt_log "Dashboard available at: $DASHBOARD_URL"
            return 0
        fi
        
        sleep 2
        attempt=$((attempt + 1))
    done
    
    agent_mgmt_error "API server failed to start within timeout"
    return 1
}

# Stop the API server
stop_api_server() {
    agent_mgmt_log "🛑 Stopping Agent Management API Server..."
    
    local pid_file="$PID_DIR/api-server.pid"
    
    if [ -f "$pid_file" ]; then
        local pid=$(cat "$pid_file")
        if kill -0 $pid 2>/dev/null; then
            kill $pid
            agent_mgmt_log "Stopped API server (PID: $pid)"
            rm -f "$pid_file"
        else
            agent_mgmt_warning "API server was not running"
            rm -f "$pid_file"
        fi
    else
        agent_mgmt_warning "No API server PID file found"
    fi
}

# Show system status
show_status() {
    agent_mgmt_log "📊 Agent Management System Status"
    echo "=================================="
    
    # Check API server
    local pid_file="$PID_DIR/api-server.pid"
    if [ -f "$pid_file" ]; then
        local pid=$(cat "$pid_file")
        if kill -0 $pid 2>/dev/null; then
            agent_mgmt_success "✅ API Server: Running (PID: $pid)"
            echo "   Dashboard: $DASHBOARD_URL"
            echo "   API: $DASHBOARD_URL/api/"
        else
            agent_mgmt_error "❌ API Server: Dead (stale PID file)"
        fi
    else
        agent_mgmt_warning "⚠️  API Server: Not running"
    fi
    
    echo ""
    
    # Try to get system status from API
    if curl -s "http://localhost:${API_PORT}/api/system/status" > /dev/null 2>&1; then
        agent_mgmt_log "📈 Live System Metrics:"
        curl -s "http://localhost:${API_PORT}/api/system/status" | python3 -c "
import json, sys
try:
    data = json.load(sys.stdin)
    agents = data.get('agents', {})
    system = data.get('system', {})
    
    print(f'   Active Agents: {agents.get(\"running\", 0)}/{agents.get(\"total\", 0)}')
    print(f'   System CPU: {system.get(\"cpu_percent\", 0):.1f}%')
    print(f'   System Memory: {system.get(\"memory_percent\", 0):.1f}%')
    print(f'   Available Memory: {system.get(\"memory_available_mb\", 0)/1024:.1f}GB')
except:
    pass
"
    else
        agent_mgmt_warning "Unable to fetch live metrics (API server may be down)"
    fi
    
    echo ""
    agent_mgmt_log "📄 Recent Log Entries:"
    
    # Show recent logs
    for log_file in "$LOG_DIR"/*.log; do
        if [ -f "$log_file" ]; then
            echo "--- $(basename "$log_file") ---"
            tail -2 "$log_file" 2>/dev/null || echo "No recent entries"
        fi
    done
}

# Open dashboard in browser
open_dashboard() {
    agent_mgmt_log "🌐 Opening Agent Management Dashboard..."
    
    # Check if API server is running
    if ! curl -s "http://localhost:${API_PORT}/api/system/status" > /dev/null 2>&1; then
        agent_mgmt_error "API server is not running. Start the system first."
        return 1
    fi
    
    # Try to open in browser
    if command -v xdg-open &> /dev/null; then
        xdg-open "$DASHBOARD_URL"
    elif command -v open &> /dev/null; then
        open "$DASHBOARD_URL"
    else
        agent_mgmt_log "Manual: Open $DASHBOARD_URL in your browser"
    fi
    
    agent_mgmt_success "Dashboard URL: $DASHBOARD_URL"
}

# Test the system
test_system() {
    agent_mgmt_log "🧪 Testing Agent Management System"
    echo "=================================="
    
    local test_results=()
    
    # Test 1: API server availability
    agent_mgmt_log "Test 1: API server availability..."
    if curl -s "http://localhost:${API_PORT}/api/system/status" > /dev/null 2>&1; then
        agent_mgmt_success "API server is responding"
        test_results+=("✅ API server")
    else
        agent_mgmt_error "API server is not responding"
        test_results+=("❌ API server")
    fi
    
    # Test 2: Agent management controller
    agent_mgmt_log "Test 2: Agent management controller..."
    if guile "$SCRIPT_DIR/agent-management-controller.scm" --status > /dev/null 2>&1; then
        agent_mgmt_success "Agent management controller is working"
        test_results+=("✅ Controller")
    else
        agent_mgmt_warning "Agent management controller has issues"
        test_results+=("⚠️  Controller")
    fi
    
    # Test 3: AtomSpace bridge
    agent_mgmt_log "Test 3: AtomSpace bridge..."
    if guile "$SCRIPT_DIR/skz-integration/bridges/atomspace-bridge.scm" --test > /dev/null 2>&1; then
        agent_mgmt_success "AtomSpace bridge is working"
        test_results+=("✅ Bridge")
    else
        agent_mgmt_warning "AtomSpace bridge has issues"
        test_results+=("⚠️  Bridge")
    fi
    
    # Test 4: Dashboard accessibility
    agent_mgmt_log "Test 4: Dashboard accessibility..."
    if curl -s "$DASHBOARD_URL" > /dev/null 2>&1; then
        agent_mgmt_success "Dashboard is accessible"
        test_results+=("✅ Dashboard")
    else
        agent_mgmt_error "Dashboard is not accessible"
        test_results+=("❌ Dashboard")
    fi
    
    echo ""
    agent_mgmt_log "📋 Test Results Summary:"
    printf '%s\n' "${test_results[@]}"
    
    # Count successful tests
    local successful=$(printf '%s\n' "${test_results[@]}" | grep -c "✅" || true)
    local total=${#test_results[@]}
    
    echo ""
    if [ $successful -eq $total ]; then
        agent_mgmt_success "🎉 All tests passed ($successful/$total)"
        return 0
    else
        agent_mgmt_warning "⚠️  Some tests failed ($successful/$total passed)"
        return 1
    fi
}

# Monitor system (continuous)
monitor_system() {
    agent_mgmt_log "👁️  Starting Agent Management System Monitor (Ctrl+C to stop)"
    echo "=================================================================="
    
    while true; do
        clear
        echo "Agent Management System Monitor - $(date)"
        echo "======================================="
        show_status
        echo ""
        echo "Press Ctrl+C to stop monitoring"
        sleep 5
    done
}

# Main function
main() {
    setup_directories
    
    case "${1:-help}" in
        "start")
            if ! check_dependencies; then
                exit 1
            fi
            initialize_system
            start_api_server
            agent_mgmt_success "🎉 Agent Management System started successfully!"
            agent_mgmt_log "Use '$0 status' to check system status"
            agent_mgmt_log "Use '$0 dashboard' to open the web interface"
            ;;
        "stop")
            stop_api_server
            agent_mgmt_success "🏁 Agent Management System stopped"
            ;;
        "restart")
            stop_api_server
            sleep 2
            if ! check_dependencies; then
                exit 1
            fi
            initialize_system
            start_api_server
            agent_mgmt_success "🔄 Agent Management System restarted"
            ;;
        "status")
            show_status
            ;;
        "dashboard")
            open_dashboard
            ;;
        "test")
            test_system
            ;;
        "monitor")
            monitor_system
            ;;
        "init")
            if ! check_dependencies; then
                exit 1
            fi
            initialize_system
            ;;
        "help"|"--help")
            echo "Agent Management System for OpenCog Cognitive Ecosystem"
            echo "======================================================="
            echo ""
            echo "Usage: $0 [command]"
            echo ""
            echo "Commands:"
            echo "  start       Start the agent management system"
            echo "  stop        Stop the agent management system"
            echo "  restart     Restart the agent management system"
            echo "  status      Show system status"
            echo "  dashboard   Open web dashboard in browser"
            echo "  test        Run system tests"
            echo "  monitor     Monitor system continuously"
            echo "  init        Initialize system without starting API server"
            echo "  help        Show this help message"
            echo ""
            echo "🧠 Agent Management Controls for SKZ Autonomous Agents Framework"
            echo "🌐 Web Dashboard: http://localhost:${API_PORT}/"
            echo "🔧 API Endpoints: http://localhost:${API_PORT}/api/"
            ;;
        *)
            agent_mgmt_error "Unknown command: $1"
            agent_mgmt_log "Use '$0 help' for usage information"
            exit 1
            ;;
    esac
}

# Execute main function with all arguments
main "$@"