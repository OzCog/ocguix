#!/bin/bash
#
# .gitpod/deploy.sh - Main deployment automation script for OpenCog Cognitive Ecosystem
# Part of the complete one-click Gitpod deployment solution
# Implements automated Guix build & deploy process with graceful fallbacks
#
# Features:
# - Automated Guix installation with fallback support
# - Comprehensive error handling and logging
# - Status reporting throughout deployment
# - Integration with existing cognitive ecosystem scripts
#

set -e

# Configuration
LOG_FILE="/tmp/opencog-deploy.log"
DEPLOYMENT_STATUS_FILE="/tmp/deployment-status.txt"
SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"
WORKSPACE_ROOT="$(dirname "$SCRIPT_DIR")"

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
PURPLE='\033[0;35m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1" | tee -a "$LOG_FILE"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1" | tee -a "$LOG_FILE"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1" | tee -a "$LOG_FILE"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1" | tee -a "$LOG_FILE"
}

log_progress() {
    echo -e "${PURPLE}[PROGRESS]${NC} $1" | tee -a "$LOG_FILE"
    echo "$1" > "$DEPLOYMENT_STATUS_FILE"
}

# Function to check if running in Gitpod
is_gitpod() {
    [ "${GITPOD_WORKSPACE_ID:-}" != "" ]
}

# Function to update deployment status
update_status() {
    local status="$1"
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $status" >> "$DEPLOYMENT_STATUS_FILE"
    log_progress "$status"
}

# Function to setup Guix with fallback
setup_guix_with_fallback() {
    update_status "Setting up GNU Guix package manager..."
    
    if command -v guix >/dev/null 2>&1; then
        log_success "Guix already installed and available"
        return 0
    fi
    
    log_info "Installing GNU Guix package manager for reproducible builds..."
    
    # Try to use the bootstrap script
    if [ -f "$WORKSPACE_ROOT/guix-cognitive-bootstrap.sh" ]; then
        log_info "Using cognitive bootstrap script..."
        if timeout 300 "$WORKSPACE_ROOT/guix-cognitive-bootstrap.sh" setup; then
            log_success "Guix installed via cognitive bootstrap"
            return 0
        else
            log_warning "Cognitive bootstrap failed, trying alternative method..."
        fi
    fi
    
    # Fallback: Manual Guix installation
    log_info "Attempting manual Guix installation..."
    if setup_guix_manual; then
        log_success "Guix installed manually"
        return 0
    fi
    
    # Final fallback: Continue without Guix
    log_warning "Guix installation failed - continuing with alternative package management"
    update_status "Continuing without Guix (using system packages)..."
    return 1
}

# Manual Guix setup function
setup_guix_manual() {
    local guix_binary_url="https://ftp.gnu.org/gnu/guix/guix-binary-1.4.0.x86_64-linux.tar.xz"
    local temp_dir="/tmp/guix-install"
    
    mkdir -p "$temp_dir"
    cd "$temp_dir"
    
    # Download with timeout and retry
    if ! timeout 120 wget -q -O guix-binary.tar.xz "$guix_binary_url"; then
        log_error "Failed to download Guix binary"
        return 1
    fi
    
    # Extract and install
    if ! tar --warning=no-timestamp -xf guix-binary.tar.xz; then
        log_error "Failed to extract Guix binary"
        return 1
    fi
    
    # Move files with error handling
    sudo mv var/guix /var/ 2>/dev/null || log_warning "Failed to move /var/guix"
    sudo mv gnu / 2>/dev/null || log_warning "Failed to move /gnu"
    
    # Setup user profile
    mkdir -p ~/.config/guix
    ln -sf /var/guix/profiles/per-user/$(whoami)/current-guix ~/.config/guix/current 2>/dev/null || true
    
    # Update PATH
    export PATH="/var/guix/profiles/per-user/$(whoami)/current-guix/bin:$PATH"
    
    # Test if guix works
    if command -v guix >/dev/null 2>&1; then
        return 0
    else
        return 1
    fi
}

# Function to install cognitive ecosystem packages
install_cognitive_packages() {
    update_status "Installing cognitive ecosystem packages..."
    
    # Try with Guix first
    if command -v guix >/dev/null 2>&1; then
        log_info "Installing packages via Guix manifest..."
        if [ -f "$SCRIPT_DIR/manifest.scm" ]; then
            if timeout 600 guix install -m "$SCRIPT_DIR/manifest.scm"; then
                log_success "Cognitive packages installed via Guix"
                return 0
            else
                log_warning "Guix package installation failed, falling back to system packages"
            fi
        else
            log_warning "Guix manifest not found, using alternative package list"
        fi
    fi
    
    # Fallback: Use system package manager
    log_info "Installing packages via system package manager..."
    sudo apt-get update -q
    
    # Essential packages for cognitive ecosystem
    local packages=(
        build-essential cmake make pkg-config
        guile-3.0 guile-3.0-dev
        python3 python3-pip python3-dev
        libboost-all-dev cxxtest
        git curl wget
        graphviz pandoc
    )
    
    if sudo apt-get install -y "${packages[@]}"; then
        log_success "System packages installed successfully"
        
        # Install Python packages
        if pip3 install requests flask fastapi uvicorn numpy scipy; then
            log_success "Python packages installed successfully"
            return 0
        else
            log_warning "Some Python packages failed to install"
            return 1
        fi
    else
        log_error "Failed to install system packages"
        return 1
    fi
}

# Function to setup cognitive environment
setup_cognitive_environment() {
    update_status "Setting up cognitive environment..."
    
    cd "$WORKSPACE_ROOT"
    
    # Make scripts executable
    chmod +x *.sh *.scm 2>/dev/null || log_warning "Some scripts could not be made executable"
    
    # Setup KoboldCpp environment
    if [ -f "./koboldcpp-setup.sh" ]; then
        log_info "Preparing KoboldCpp environment..."
        if timeout 300 ./koboldcpp-setup.sh --setup-only; then
            log_success "KoboldCpp environment prepared"
        else
            log_warning "KoboldCpp setup encountered issues"
        fi
    fi
    
    # Validate cognitive agents
    if [ -f "./cognitive-grammar-integration-agent.scm" ]; then
        log_info "Validating cognitive grammar agent..."
        if ./cognitive-grammar-integration-agent.scm --validate 2>/dev/null; then
            log_success "Cognitive grammar agent validated"
        else
            log_warning "Cognitive grammar agent validation failed"
        fi
    fi
    
    log_success "Cognitive environment setup completed"
}

# Function to start services
start_cognitive_services() {
    update_status "Starting cognitive services..."
    
    # Start KoboldCpp server in background
    if [ -f "$WORKSPACE_ROOT/koboldcpp-setup.sh" ]; then
        log_info "Starting KoboldCpp server..."
        nohup "$WORKSPACE_ROOT/koboldcpp-setup.sh" --start-only > /tmp/koboldcpp.log 2>&1 &
        echo $! > /tmp/koboldcpp.pid
        log_success "KoboldCpp server started (PID: $(cat /tmp/koboldcpp.pid))"
    fi
    
    # Wait for services to be ready
    update_status "Waiting for services to become ready..."
    sleep 10
    
    # Check if KoboldCpp is responding
    if curl -s http://localhost:5001 >/dev/null 2>&1; then
        log_success "KoboldCpp server is responding on port 5001"
    else
        log_warning "KoboldCpp server may not be ready yet"
    fi
}

# Function to display deployment summary
display_summary() {
    update_status "Deployment completed!"
    
    echo ""
    echo "🧠 OpenCog Cognitive Ecosystem - Deployment Summary"
    echo "=================================================="
    echo ""
    
    if is_gitpod; then
        echo "🌍 Gitpod Environment: $GITPOD_WORKSPACE_URL"
        echo "🐳 Workspace ID: $GITPOD_WORKSPACE_ID"
    fi
    
    echo "📍 Available Services:"
    echo "  • KoboldCpp Web Interface: http://localhost:5001"
    echo "  • Cognitive Grammar Agent: ./cognitive-grammar-integration-agent.scm"
    echo "  • Package Management: ./ocpkg"
    echo ""
    
    echo "🚀 Quick Start Commands:"
    echo "  • Test cognitive pipeline: ./test-cognitive-flowchart.sh"
    echo "  • Run demo: ./demo-cognitive-flowchart.sh"
    echo "  • Check status: cat $DEPLOYMENT_STATUS_FILE"
    echo ""
    
    echo "📋 Documentation:"
    echo "  • Architecture: TECHNICAL-ARCHITECTURE.md"
    echo "  • Troubleshooting: .gitpod/TROUBLESHOOTING.md"
    echo "  • Deployment logs: $LOG_FILE"
    echo ""
    
    log_success "OpenCog Cognitive Ecosystem is ready for use!"
}

# Main deployment function
main() {
    # Initialize logging
    echo "$(date '+%Y-%m-%d %H:%M:%S') - Starting OpenCog deployment..." > "$LOG_FILE"
    update_status "Initializing OpenCog Cognitive Ecosystem deployment..."
    
    log_info "🧠 OpenCog Cognitive Ecosystem - Automated Deployment"
    log_info "===================================================="
    
    if is_gitpod; then
        log_info "🐳 Running in Gitpod environment"
        log_info "📍 Workspace: $GITPOD_WORKSPACE_URL"
    else
        log_info "🖥️  Running in local environment"
    fi
    
    echo ""
    
    # Run deployment steps
    setup_guix_with_fallback || log_warning "Continuing without Guix"
    install_cognitive_packages
    setup_cognitive_environment
    start_cognitive_services
    display_summary
    
    # Final status
    update_status "Deployment completed successfully - Cognitive ecosystem ready!"
    exit 0
}

# Error handling
trap 'log_error "Deployment failed at line $LINENO"; update_status "Deployment failed - Check logs at $LOG_FILE"; exit 1' ERR

# Run main function
main "$@"