#!/bin/bash
#
# clone-opencog-repos.sh - Clone the 19 OpenCog repositories needed for the monorepo
# This script clones the missing repositories from the OpenCog organization
#
# Usage: ./clone-opencog-repos.sh
#

set -e

# Configuration
REPOS_DIR="repos"
OPENCOG_ORG="https://github.com/opencog"

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# List of 19 repositories from the oc.yml workflow
REQUIRED_REPOS=(
    "cogutil"
    "atomspace"
    "atomspace-storage"
    "atomspace-rocks"
    "atomspace-restful"
    "cogserver"
    "unify"
    "ure"
    "spacetime"
    "attention"
    "miner"
    "pln"
    "moses"
    "asmoses"
    "lg-atomese"
    "learn"
    "pattern-index"
    "vision"
    "opencog"
)

# Function to check if a repository exists
repo_exists() {
    local repo_name="$1"
    [ -d "$repo_name" ]
}

# Function to clone a repository with error handling
clone_repo() {
    local repo_name="$1"
    local repo_url="$OPENCOG_ORG/$repo_name.git"
    
    log_info "Cloning $repo_name from $repo_url..."
    
    if git clone --depth 1 "$repo_url" "$repo_name"; then
        log_success "Successfully cloned $repo_name"
        return 0
    else
        log_error "Failed to clone $repo_name"
        return 1
    fi
}

# Main function
main() {
    log_info "OpenCog Repository Cloning Script"
    log_info "================================="
    echo ""
    
    # Create repos directory if it doesn't exist
    if [ ! -d "$REPOS_DIR" ]; then
        log_info "Creating $REPOS_DIR directory..."
        mkdir -p "$REPOS_DIR"
    fi
    
    cd "$REPOS_DIR" || {
        log_error "Failed to change to $REPOS_DIR directory"
        exit 1
    }
    
    local cloned_count=0
    local failed_count=0
    local existing_count=0
    
    log_info "Checking and cloning required repositories..."
    echo ""
    
    for repo in "${REQUIRED_REPOS[@]}"; do
        if repo_exists "$repo"; then
            log_success "$repo already exists, skipping..."
            ((existing_count++))
        else
            if clone_repo "$repo"; then
                ((cloned_count++))
            else
                ((failed_count++))
            fi
        fi
    done
    
    cd ..
    
    echo ""
    log_info "Repository cloning summary:"
    log_info "=========================="
    log_success "Existing repositories: $existing_count"
    log_success "Successfully cloned: $cloned_count"
    if [ $failed_count -gt 0 ]; then
        log_error "Failed to clone: $failed_count"
    fi
    
    echo ""
    log_info "Current repositories in $REPOS_DIR:"
    for dir in */; do
        dir=${dir%/}  # Remove trailing slash
        if [[ " ${REQUIRED_REPOS[*]} " =~ " ${dir} " ]]; then
            log_success "✓ $dir (required)"
        else
            log_warning "• $dir (extra)"
        fi
    done
    
    if [ $failed_count -eq 0 ]; then
        echo ""
        log_success "All required repositories are now available!"
        log_info "You can now run the build workflow or gitpod deployment."
    else
        echo ""
        log_warning "Some repositories failed to clone. Check network connection and try again."
        exit 1
    fi
}

# Run main function
main "$@"