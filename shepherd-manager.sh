#!/bin/bash
# Helper script for managing OpenCog Shepherd services in the devcontainer

set -e

# Ensure we're in the right environment
if [ ! -f "/home/vscode/.config/guix/current/bin/guix" ]; then
    echo "Error: Guix not found. Please ensure the devcontainer is properly set up."
    exit 1
fi

# Source Guix environment
source /home/vscode/.config/guix/current/etc/profile
export PATH="/home/vscode/.config/guix/current/bin:/home/vscode/.guix-profile/bin:${PATH}"

# Change to workspace directory
cd /workspace

usage() {
    echo "Usage: $0 {start|stop|status|restart|build|test|help}"
    echo ""
    echo "Commands:"
    echo "  start     - Start shepherd daemon and services"
    echo "  stop      - Stop shepherd daemon"
    echo "  status    - Show shepherd and service status"
    echo "  restart   - Restart shepherd daemon"
    echo "  build     - Trigger OpenCog build via shepherd"
    echo "  test      - Run OpenCog tests via shepherd"
    echo "  help      - Show this help message"
}

start_shepherd() {
    echo "Starting shepherd daemon..."
    if pgrep shepherd >/dev/null 2>&1; then
        echo "Shepherd is already running"
    else
        shepherd --config=/workspace/.config/shepherd/init.scm &
        sleep 2
        echo "Shepherd started"
    fi
    
    echo "Available services:"
    herd status || true
}

stop_shepherd() {
    echo "Stopping shepherd daemon..."
    if pgrep shepherd >/dev/null 2>&1; then
        herd stop root
        pkill shepherd || true
        echo "Shepherd stopped"
    else
        echo "Shepherd is not running"
    fi
}

status_shepherd() {
    echo "Shepherd status:"
    if pgrep shepherd >/dev/null 2>&1; then
        echo "Shepherd daemon: Running (PID: $(pgrep shepherd))"
        echo ""
        echo "Services:"
        herd status || echo "Could not get service status"
    else
        echo "Shepherd daemon: Stopped"
    fi
}

build_opencog() {
    echo "Triggering OpenCog build..."
    if ! pgrep shepherd >/dev/null 2>&1; then
        start_shepherd
    fi
    
    herd start opencog-build || echo "Build service may already be running or failed"
    echo "Check logs: tail -f /tmp/opencog-build.log"
}

test_opencog() {
    echo "Running OpenCog tests..."
    if ! pgrep shepherd >/dev/null 2>&1; then
        start_shepherd
    fi
    
    herd start opencog-test || echo "Test service may already be running or failed"
    echo "Check logs: tail -f /tmp/opencog-test.log"
}

case "$1" in
    start)
        start_shepherd
        ;;
    stop)
        stop_shepherd
        ;;
    status)
        status_shepherd
        ;;
    restart)
        stop_shepherd
        sleep 1
        start_shepherd
        ;;
    build)
        build_opencog
        ;;
    test)
        test_opencog
        ;;
    help|--help|-h)
        usage
        ;;
    *)
        echo "Error: Unknown command '$1'"
        echo ""
        usage
        exit 1
        ;;
esac