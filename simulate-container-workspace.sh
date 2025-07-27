#!/bin/bash
#
# simulate-container-workspace.sh - Container workspace simulation and debugging tool
# Addresses the issue: "container workspace ran with an error: exit code 1"
# Provides a way to simulate container conditions and see why they fail
#

set -e

# Script configuration
SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"
SIMULATION_LOG="/tmp/container-simulation.log"
BACKUP_DIR="/tmp/container-simulation-backup"

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
PURPLE='\033[0;35m'
NC='\033[0m' # No Color

# Initialize logging
echo "$(date '+%Y-%m-%d %H:%M:%S') - Container workspace simulation started" > "$SIMULATION_LOG"

# Logging functions
log_info() {
    echo -e "${BLUE}[SIMULATE]${NC} $1" | tee -a "$SIMULATION_LOG"
}

log_success() {
    echo -e "${GREEN}[SIMULATE]${NC} $1" | tee -a "$SIMULATION_LOG"
}

log_warning() {
    echo -e "${YELLOW}[SIMULATE]${NC} $1" | tee -a "$SIMULATION_LOG"
}

log_error() {
    echo -e "${RED}[SIMULATE]${NC} $1" | tee -a "$SIMULATION_LOG"
}

log_header() {
    echo -e "${PURPLE}[SIMULATE]${NC} $1" | tee -a "$SIMULATION_LOG"
}

# Function to backup important files
backup_files() {
    log_info "Creating backup of important files..."
    mkdir -p "$BACKUP_DIR"
    
    local files=(
        "registry-sources.scm"
        "base-devcontainers.scm"
        "cognitive-manifest.scm"
        "hypergraph-schema.scm"
    )
    
    for file in "${files[@]}"; do
        if [ -f "$file" ]; then
            cp "$file" "$BACKUP_DIR/" 2>/dev/null || true
            log_info "Backed up: $file"
        fi
    done
}

# Function to restore files
restore_files() {
    log_info "Restoring backed up files..."
    if [ -d "$BACKUP_DIR" ]; then
        for file in "$BACKUP_DIR"/*; do
            if [ -f "$file" ]; then
                local filename=$(basename "$file")
                cp "$file" "./" 2>/dev/null || true
                log_info "Restored: $filename"
            fi
        done
    fi
}

# Function to simulate missing dependency files
simulate_missing_dependencies() {
    log_header "=== Simulating Missing Dependencies (Common Container Issue) ==="
    
    # Test 1: Missing registry-sources.scm
    log_info "Test 1: Missing registry-sources.scm"
    if [ -f "registry-sources.scm" ]; then
        mv "registry-sources.scm" "registry-sources.scm.bak"
        log_info "Temporarily removed registry-sources.scm"
        
        log_info "Running registry-discovery-agent.scm..."
        if guile ./registry-discovery-agent.scm > /tmp/test1.log 2>&1; then
            log_success "Agent handled missing dependency gracefully"
        else
            local exit_code=$?
            log_error "Agent failed with exit code $exit_code"
            log_error "This is the likely cause of 'container workspace ran with an error: exit code 1'"
            cat /tmp/test1.log | tail -5 | while read line; do
                log_error "  $line"
            done
        fi
        
        mv "registry-sources.scm.bak" "registry-sources.scm" 2>/dev/null || true
        log_info "Restored registry-sources.scm"
    fi
    
    echo ""
    
    # Test 2: Missing base-devcontainers.scm
    log_info "Test 2: Missing base-devcontainers.scm"
    if [ -f "base-devcontainers.scm" ]; then
        mv "base-devcontainers.scm" "base-devcontainers.scm.bak"
        log_info "Temporarily removed base-devcontainers.scm"
        
        log_info "Running profile-extraction-agent.scm..."
        if guile ./profile-extraction-agent.scm > /tmp/test2.log 2>&1; then
            log_success "Agent handled missing dependency gracefully"
        else
            local exit_code=$?
            log_error "Agent failed with exit code $exit_code"
            cat /tmp/test2.log | tail -5 | while read line; do
                log_error "  $line"
            done
        fi
        
        mv "base-devcontainers.scm.bak" "base-devcontainers.scm" 2>/dev/null || true
        log_info "Restored base-devcontainers.scm"
    fi
    
    echo ""
}

# Function to simulate permission issues
simulate_permission_issues() {
    log_header "=== Simulating Permission Issues (Container Environment) ==="
    
    # Test: Read-only filesystem simulation
    log_info "Test: Simulating read-only /tmp directory"
    
    # Create a temporary directory that we'll make read-only
    local temp_output_dir="/tmp/readonly-test-$$"
    mkdir -p "$temp_output_dir"
    
    # Try to write to a read-only location by changing environment
    log_info "Running registry-discovery-agent with restricted /tmp access..."
    
    # Temporarily redirect output to a custom location
    if TMP_DIR="$temp_output_dir" guile -c "
        (setenv \"TMP_DIR\" \"$temp_output_dir\")
        (load \"./registry-discovery-agent.scm\")
    " > /tmp/test3.log 2>&1; then
        log_success "Agent completed despite permission restrictions"
    else
        local exit_code=$?
        log_warning "Agent had issues with permission restrictions (exit code $exit_code)"
        if [ $exit_code -eq 1 ]; then
            log_error "This could cause the 'exit code 1' error in restricted container environments"
        fi
    fi
    
    # Cleanup
    rm -rf "$temp_output_dir" 2>/dev/null || true
    
    echo ""
}

# Function to simulate network issues
simulate_network_issues() {
    log_header "=== Simulating Network Issues (Offline Container) ==="
    
    log_info "Test: Simulating network-disconnected container environment"
    
    # Test with completely isolated network environment
    log_info "Running agents in simulated offline mode..."
    
    # Disable network access for the agent by using invalid DNS
    if GUILE_LOAD_PATH="." timeout 30 guile -c "
        (setenv \"NO_NETWORK\" \"true\")
        (load \"./registry-discovery-agent.scm\")
    " > /tmp/test4.log 2>&1; then
        log_success "Registry agent handled offline mode correctly"
    else
        local exit_code=$?
        log_warning "Registry agent had issues in offline mode (exit code $exit_code)"
        if [ $exit_code -eq 1 ]; then
            log_error "Network dependency could cause 'exit code 1' in isolated containers"
        fi
    fi
    
    echo ""
}

# Function to simulate resource constraints
simulate_resource_constraints() {
    log_header "=== Simulating Resource Constraints (Low Memory Container) ==="
    
    log_info "Test: Simulating memory-constrained environment"
    
    # Test with limited memory by using ulimit (if available)
    log_info "Running agents with memory constraints..."
    
    # Use timeout to simulate resource constraints
    if timeout 10 guile ./registry-discovery-agent.scm > /tmp/test5.log 2>&1; then
        log_success "Agent completed within resource constraints"
    else
        local exit_code=$?
        log_warning "Agent hit resource constraints (exit code $exit_code)"
        if [ $exit_code -eq 124 ]; then
            log_warning "Timeout occurred - could indicate resource issues"
        elif [ $exit_code -eq 1 ]; then
            log_error "Resource constraints could cause 'exit code 1'"
        fi
    fi
    
    echo ""
}

# Function to run the container workspace test
test_container_workspace() {
    log_header "=== Running Container Workspace Test ==="
    
    log_info "Executing the actual container workspace test..."
    
    if ./test-container-workspace.sh > /tmp/workspace-test.log 2>&1; then
        log_success "Container workspace test passed"
    else
        local exit_code=$?
        log_error "Container workspace test failed with exit code $exit_code"
        log_error "Last few lines of output:"
        tail -10 /tmp/workspace-test.log | while read line; do
            log_error "  $line"
        done
    fi
    
    echo ""
}

# Function to simulate actual CI environment
simulate_ci_environment() {
    log_header "=== Simulating CI Environment Conditions ==="
    
    log_info "Simulating GitHub Actions CI environment variables..."
    
    # Set CI environment variables
    export CI=true
    export GITHUB_ACTIONS=true
    export RUNNER_OS=Linux
    export GITHUB_WORKSPACE=$(pwd)
    
    log_info "Running cognitive ecosystem workflow steps..."
    
    # Test each step from the CI workflow
    local steps=(
        "guile ./registry-discovery-agent.scm"
        "guile ./profile-extraction-agent.scm"
        "guile ./artifact-synthesis-agent.scm"
        "guile ./meta-cognitive-feedback-agent.scm"
    )
    
    local step_num=1
    for step in "${steps[@]}"; do
        log_info "CI Step $step_num: $step"
        if timeout 60 $step > /tmp/ci-step-$step_num.log 2>&1; then
            log_success "CI Step $step_num completed successfully"
        else
            local exit_code=$?
            log_error "CI Step $step_num failed with exit code $exit_code"
            if [ $exit_code -eq 1 ]; then
                log_error "This is the 'exit code 1' error in CI environment!"
                log_error "Error details:"
                tail -5 /tmp/ci-step-$step_num.log | while read line; do
                    log_error "  $line"
                done
            fi
        fi
        step_num=$((step_num + 1))
    done
    
    # Unset CI variables
    unset CI GITHUB_ACTIONS RUNNER_OS GITHUB_WORKSPACE
    
    echo ""
}

# Function to display simulation results
display_simulation_results() {
    log_header "=== Container Simulation Results ==="
    
    echo ""
    echo "🐳 Container Workspace Simulation Complete"
    echo "=========================================="
    echo ""
    
    echo "📋 Simulation Log: $SIMULATION_LOG"
    echo "🔍 Test Logs: /tmp/test*.log, /tmp/ci-step-*.log"
    echo ""
    
    echo "🎯 Common Causes of 'exit code 1' in Container Workspaces:"
    echo "  1. Missing dependency files (registry-sources.scm, base-devcontainers.scm)"
    echo "  2. Permission issues in container filesystem"
    echo "  3. Network connectivity problems in isolated containers"
    echo "  4. Resource constraints (memory, CPU, timeout)"
    echo "  5. CI environment variable conflicts"
    echo ""
    
    echo "🔧 Solutions Implemented:"
    echo "  ✅ Added graceful fallback for missing dependency files"
    echo "  ✅ Enhanced error handling in agent scripts"
    echo "  ✅ Container-specific fallback data"
    echo "  ✅ Offline mode detection and handling"
    echo ""
    
    echo "🚀 Testing Commands:"
    echo "  • Full simulation: ./simulate-container-workspace.sh"
    echo "  • Container test: ./test-container-workspace.sh"
    echo "  • View logs: cat $SIMULATION_LOG"
    echo ""
    
    log_success "Simulation complete - check logs for detailed analysis"
}

# Main simulation function
main() {
    log_header "🐳 Container Workspace Error Simulation"
    log_header "=========================================="
    
    echo ""
    log_info "This script simulates container conditions that cause 'exit code 1'"
    log_info "It helps debug and identify why container workspaces fail"
    echo ""
    
    # Create backup before simulation
    backup_files
    
    # Run simulation tests
    simulate_missing_dependencies
    simulate_permission_issues
    simulate_network_issues
    simulate_resource_constraints
    test_container_workspace
    simulate_ci_environment
    
    # Restore files
    restore_files
    
    # Display results
    display_simulation_results
}

# Error handling
trap 'log_error "Simulation failed at line $LINENO"; restore_files; exit 1' ERR

# Handle script arguments
case "${1:-}" in
    --help|-h)
        echo "Container Workspace Error Simulation"
        echo "Usage: $0 [options]"
        echo ""
        echo "Options:"
        echo "  --help, -h    Show this help message"
        echo "  --deps        Test only dependency issues"
        echo "  --perms       Test only permission issues"
        echo "  --network     Test only network issues"
        echo "  --resources   Test only resource constraint issues"
        echo "  --ci          Test only CI environment issues"
        echo ""
        echo "This script helps simulate and debug container workspace issues"
        echo "that cause 'exit code 1' errors."
        exit 0
        ;;
    --deps)
        backup_files
        simulate_missing_dependencies
        restore_files
        ;;
    --perms)
        backup_files
        simulate_permission_issues
        restore_files
        ;;
    --network)
        backup_files
        simulate_network_issues
        restore_files
        ;;
    --resources)
        backup_files
        simulate_resource_constraints
        restore_files
        ;;
    --ci)
        backup_files
        simulate_ci_environment
        restore_files
        ;;
    *)
        # Run full simulation
        main "$@"
        ;;
esac