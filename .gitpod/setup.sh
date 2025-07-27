#!/bin/bash
#
# .gitpod/setup.sh - Environment setup script for OpenCog Cognitive Ecosystem
# Prepares the workspace environment before main deployment
#
# This script handles:
# - Initial environment validation
# - Workspace permissions and directories
# - Basic tool availability checks
# - Pre-deployment optimizations
#

set -e

# Configuration
SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"
WORKSPACE_ROOT="$(dirname "$SCRIPT_DIR")"
LOG_FILE="/tmp/opencog-setup.log"

# Color codes
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

# Logging functions
log_info() {
    echo -e "${BLUE}[SETUP]${NC} $1" | tee -a "$LOG_FILE"
}

log_success() {
    echo -e "${GREEN}[SETUP]${NC} $1" | tee -a "$LOG_FILE"
}

log_warning() {
    echo -e "${YELLOW}[SETUP]${NC} $1" | tee -a "$LOG_FILE"
}

log_error() {
    echo -e "${RED}[SETUP]${NC} $1" | tee -a "$LOG_FILE"
}

# Function to check if running in Gitpod
is_gitpod() {
    [ "${GITPOD_WORKSPACE_ID:-}" != "" ]
}

# Function to validate environment
validate_environment() {
    log_info "Validating workspace environment..."
    
    # Check basic tools
    local required_tools=("git" "curl" "wget" "python3" "bash")
    local missing_tools=()
    
    for tool in "${required_tools[@]}"; do
        if ! command -v "$tool" >/dev/null 2>&1; then
            missing_tools+=("$tool")
        fi
    done
    
    if [ ${#missing_tools[@]} -gt 0 ]; then
        log_warning "Missing tools: ${missing_tools[*]}"
        log_info "These will be installed during deployment"
    else
        log_success "All basic tools are available"
    fi
    
    # Check workspace structure
    if [ -d "$WORKSPACE_ROOT" ]; then
        log_success "Workspace root found: $WORKSPACE_ROOT"
    else
        log_error "Workspace root not found"
        return 1
    fi
    
    # Check for key files
    local key_files=(
        "koboldcpp-setup.sh"
        "cognitive-manifest.scm"
        "cognitive-grammar-integration-agent.scm"
        "ocpkg"
    )
    
    for file in "${key_files[@]}"; do
        if [ -f "$WORKSPACE_ROOT/$file" ]; then
            log_success "Found: $file"
        else
            log_warning "Missing: $file"
        fi
    done
}

# Function to setup workspace permissions
setup_permissions() {
    log_info "Setting up workspace permissions..."
    
    cd "$WORKSPACE_ROOT"
    
    # Make scripts executable
    find . -name "*.sh" -type f -exec chmod +x {} \; 2>/dev/null || true
    find . -name "*.scm" -type f -exec chmod +x {} \; 2>/dev/null || true
    
    # Specific important scripts
    local scripts=(
        "ocpkg"
        "octool-wip"
        "koboldcpp-setup.sh"
        "guix-cognitive-bootstrap.sh"
        "cognitive-grammar-integration-agent.scm"
    )
    
    for script in "${scripts[@]}"; do
        if [ -f "$script" ]; then
            chmod +x "$script"
            log_success "Made executable: $script"
        fi
    done
    
    # Setup temporary directories
    mkdir -p /tmp/opencog-workspace
    mkdir -p /tmp/koboldcpp-cache
    mkdir -p /tmp/cognitive-agents
    
    log_success "Workspace permissions configured"
}

# Function to setup environment variables
setup_environment_variables() {
    log_info "Setting up environment variables..."
    
    # Create environment file
    cat > /tmp/opencog-env.sh << 'EOF'
#!/bin/bash
# OpenCog Cognitive Ecosystem Environment Variables

# Core settings
export OPENCOG_ECOSYSTEM=true
export KOBOLDCPP_PORT=5001
export COGNITIVE_GRAMMAR_AGENT=distributed-cognitive-grammar-agent.scm
export GITPOD_WORKSPACE_TYPE=cognitive-ecosystem

# Paths
export OPENCOG_WORKSPACE=/workspace
export KOGNITIVO_LOG_DIR=/tmp
export COGNITIVE_MANIFEST_PATH=./cognitive-manifest.scm

# Performance optimizations
export PYTHONUNBUFFERED=1
export PYTHONDONTWRITEBYTECODE=1

# Guix settings
export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"
export GUIX_PROFILE="$HOME/.guix-profile"

# Development settings
export OPENCOG_BUILD_TYPE=RelWithDebInfo
export OPENCOG_ENABLE_TESTS=true

# Network settings for cognitive agents
export COGNITIVE_NETWORK_TIMEOUT=30
export KOBOLDCPP_RETRIES=3

EOF
    
    # Source the environment
    source /tmp/opencog-env.sh
    
    log_success "Environment variables configured"
}

# Function to optimize workspace for Gitpod
optimize_for_gitpod() {
    if ! is_gitpod; then
        log_info "Not in Gitpod, skipping Gitpod-specific optimizations"
        return 0
    fi
    
    log_info "Applying Gitpod-specific optimizations..."
    
    # Set Gitpod-specific environment variables
    export GITPOD_WORKSPACE_ROOT="$WORKSPACE_ROOT"
    export GITPOD_REPO_ROOT="$WORKSPACE_ROOT"
    
    # Create Gitpod-specific symlinks
    ln -sf "$WORKSPACE_ROOT" /workspace/ocguix 2>/dev/null || true
    
    # Setup port forwarding info
    cat > /tmp/gitpod-ports.txt << EOF
Gitpod Port Configuration:
- 5001: KoboldCpp Web Interface (HTTP)
- 8080: Alternative service port
- 3000: Development server port

Access URLs:
- KoboldCpp: https://5001-$GITPOD_WORKSPACE_ID.ws-$GITPOD_WORKSPACE_CLUSTER_HOST
EOF
    
    log_success "Gitpod optimizations applied"
}

# Function to pre-download common resources
pre_download_resources() {
    log_info "Pre-downloading common resources..."
    
    # Create download cache
    mkdir -p /tmp/opencog-cache
    
    # Download commonly used files in background
    (
        # Small model for testing (only if network is available)
        if curl -s --connect-timeout 5 https://httpbin.org/status/200 >/dev/null 2>&1; then
            log_info "Network available, pre-downloading resources..."
            
            # Pre-fetch KoboldCpp releases info
            curl -s https://api.github.com/repos/LostRuins/koboldcpp/releases/latest > /tmp/koboldcpp-release.json 2>/dev/null || true
            
            log_success "Resource pre-download completed"
        else
            log_warning "Network not available, skipping resource pre-download"
        fi
    ) &
}

# Function to display setup summary
display_setup_summary() {
    log_info "Setup completed successfully!"
    
    echo ""
    echo "🔧 OpenCog Cognitive Ecosystem - Setup Summary"
    echo "=============================================="
    echo ""
    
    if is_gitpod; then
        echo "🐳 Environment: Gitpod Workspace"
        echo "📍 Workspace ID: $GITPOD_WORKSPACE_ID"
    else
        echo "🖥️  Environment: Local Development"
    fi
    
    echo "📁 Workspace: $WORKSPACE_ROOT"
    echo "📋 Setup Log: $LOG_FILE"
    echo "🌍 Environment: /tmp/opencog-env.sh"
    echo ""
    
    echo "✅ Next Steps:"
    echo "  1. Run main deployment: .gitpod/deploy.sh"
    echo "  2. Or use integrated Gitpod tasks (automatic)"
    echo ""
    
    log_success "Environment setup ready for deployment!"
}

# Main setup function
main() {
    # Initialize logging
    echo "$(date '+%Y-%m-%d %H:%M:%S') - Starting environment setup..." > "$LOG_FILE"
    
    log_info "🔧 OpenCog Cognitive Ecosystem - Environment Setup"
    log_info "================================================"
    
    if is_gitpod; then
        log_info "🐳 Configuring Gitpod environment"
    else
        log_info "🖥️  Configuring local environment"
    fi
    
    echo ""
    
    # Run setup steps
    validate_environment
    setup_permissions
    setup_environment_variables
    optimize_for_gitpod
    pre_download_resources
    display_setup_summary
    
    log_success "Environment setup completed successfully!"
    exit 0
}

# Error handling
trap 'log_error "Setup failed at line $LINENO"; exit 1' ERR

# Run main function
main "$@"