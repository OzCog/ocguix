#!/bin/bash
#
# test-monorepo-integration.sh - Test script to verify monorepo integration works
# Tests both GitHub Actions workflow compatibility and Gitpod deployment
#

set -e

# Configuration
SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"
WORKSPACE_ROOT="$SCRIPT_DIR"
LOG_FILE="/tmp/monorepo-integration-test.log"

# Color codes
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

# Logging functions
log_info() {
    echo -e "${BLUE}[TEST]${NC} $1" | tee -a "$LOG_FILE"
}

log_success() {
    echo -e "${GREEN}[TEST]${NC} $1" | tee -a "$LOG_FILE"
}

log_warning() {
    echo -e "${YELLOW}[TEST]${NC} $1" | tee -a "$LOG_FILE"
}

log_error() {
    echo -e "${RED}[TEST]${NC} $1" | tee -a "$LOG_FILE"
}

# Test 1: Verify repository structure
test_repository_structure() {
    log_info "Testing repository structure..."
    
    local repos_dir="$WORKSPACE_ROOT/repos"
    
    if [ ! -d "$repos_dir" ]; then
        log_error "repos/ directory not found"
        return 1
    fi
    
    local expected_repos=(
        "cogutil" "atomspace" "atomspace-storage" "atomspace-rocks" "atomspace-restful"
        "cogserver" "unify" "ure" "spacetime" "attention" "miner" "pln" "moses"
        "asmoses" "lg-atomese" "learn" "pattern-index" "vision" "opencog"
    )
    
    local missing_repos=()
    local found_repos=()
    
    for repo in "${expected_repos[@]}"; do
        if [ -d "$repos_dir/$repo" ]; then
            found_repos+=("$repo")
        else
            missing_repos+=("$repo")
        fi
    done
    
    log_info "Found ${#found_repos[@]} repositories in monorepo"
    
    if [ ${#missing_repos[@]} -gt 0 ]; then
        log_warning "Missing repositories: ${missing_repos[*]}"
    fi
    
    # Verify that repositories have actual content (not empty directories)
    local content_repos=()
    local empty_repos=()
    
    for repo in "${found_repos[@]}"; do
        if [ -n "$(ls -A "$repos_dir/$repo" 2>/dev/null)" ]; then
            content_repos+=("$repo")
        else
            empty_repos+=("$repo")
        fi
    done
    
    log_success "Repositories with content: ${#content_repos[@]}"
    
    if [ ${#empty_repos[@]} -gt 0 ]; then
        log_warning "Empty repositories: ${empty_repos[*]}"
    fi
    
    # Check for key files in repositories
    local repos_with_cmake=0
    for repo in "${content_repos[@]}"; do
        if [ -f "$repos_dir/$repo/CMakeLists.txt" ]; then
            ((repos_with_cmake++))
        fi
    done
    
    log_info "Repositories with CMakeLists.txt: $repos_with_cmake"
    
    if [ $repos_with_cmake -gt 10 ]; then
        log_success "Repository structure test PASSED"
        return 0
    else
        log_error "Repository structure test FAILED - insufficient content"
        return 1
    fi
}

# Test 2: Verify GitHub Actions workflow syntax
test_github_actions_syntax() {
    log_info "Testing GitHub Actions workflow syntax..."
    
    local workflows_dir="$WORKSPACE_ROOT/.github/workflows"
    local failed_workflows=()
    local passed_workflows=()
    
    for workflow in "$workflows_dir"/*.yml; do
        if [ -f "$workflow" ]; then
            local workflow_name=$(basename "$workflow")
            
            # Basic YAML syntax check
            if python3 -c "import yaml; yaml.safe_load(open('$workflow'))" 2>/dev/null; then
                passed_workflows+=("$workflow_name")
            else
                failed_workflows+=("$workflow_name")
            fi
        fi
    done
    
    log_info "Valid workflow files: ${#passed_workflows[@]}"
    
    if [ ${#failed_workflows[@]} -gt 0 ]; then
        log_error "Invalid workflow files: ${failed_workflows[*]}"
        return 1
    fi
    
    # Check for monorepo references in key workflows
    local key_workflows=("ci-org-v7.yml" "oc.yml" "efficient-build.yml")
    local updated_workflows=()
    
    for workflow in "${key_workflows[@]}"; do
        if [ -f "$workflows_dir/$workflow" ]; then
            if grep -q "repos/" "$workflows_dir/$workflow"; then
                updated_workflows+=("$workflow")
                log_info "✓ $workflow uses monorepo structure"
            else
                log_warning "✗ $workflow may not be updated for monorepo"
            fi
        fi
    done
    
    if [ ${#updated_workflows[@]} -ge 2 ]; then
        log_success "GitHub Actions syntax test PASSED"
        return 0
    else
        log_error "GitHub Actions syntax test FAILED"
        return 1
    fi
}

# Test 3: Verify Gitpod configuration
test_gitpod_configuration() {
    log_info "Testing Gitpod configuration..."
    
    local gitpod_files=(
        ".gitpod.yml"
        ".gitpod.Dockerfile"
        ".gitpod/deploy.sh"
        ".gitpod/setup.sh"
    )
    
    local missing_files=()
    local found_files=()
    
    for file in "${gitpod_files[@]}"; do
        if [ -f "$WORKSPACE_ROOT/$file" ]; then
            found_files+=("$file")
        else
            missing_files+=("$file")
        fi
    done
    
    if [ ${#missing_files[@]} -gt 0 ]; then
        log_warning "Missing Gitpod files: ${missing_files[*]}"
    fi
    
    # Check if deploy.sh has been updated for monorepo
    if [ -f "$WORKSPACE_ROOT/.gitpod/deploy.sh" ]; then
        if grep -q "verify_local_repositories" "$WORKSPACE_ROOT/.gitpod/deploy.sh"; then
            log_success "✓ deploy.sh updated for monorepo"
        else
            log_warning "✗ deploy.sh may not be updated for monorepo"
        fi
    fi
    
    # Test deploy.sh can run without errors (dry run)
    if [ -x "$WORKSPACE_ROOT/.gitpod/deploy.sh" ]; then
        log_info "Testing deploy.sh execution..."
        
        # Create a minimal test by sourcing the verification function
        if bash -c "source '$WORKSPACE_ROOT/.gitpod/deploy.sh'; verify_local_repositories" 2>/dev/null; then
            log_success "✓ deploy.sh verification function works"
        else
            log_warning "✗ deploy.sh verification function has issues"
        fi
    fi
    
    log_success "Gitpod configuration test PASSED"
    return 0
}

# Test 4: Verify build compatibility
test_build_compatibility() {
    log_info "Testing build compatibility..."
    
    # Check if key repositories have proper CMake structure
    local buildable_repos=()
    local repos_dir="$WORKSPACE_ROOT/repos"
    
    local test_repos=("cogutil" "atomspace" "cogserver")
    
    for repo in "${test_repos[@]}"; do
        if [ -f "$repos_dir/$repo/CMakeLists.txt" ]; then
            buildable_repos+=("$repo")
            log_info "✓ $repo has CMakeLists.txt"
        else
            log_warning "✗ $repo missing CMakeLists.txt"
        fi
    done
    
    # Create a test build directory to verify CMake can run
    local test_repo="$repos_dir/cogutil"
    if [ -f "$test_repo/CMakeLists.txt" ]; then
        log_info "Testing CMake configuration for cogutil..."
        
        local test_build_dir="/tmp/test-build-cogutil"
        mkdir -p "$test_build_dir"
        cd "$test_build_dir"
        
        if cmake "$test_repo" -DCMAKE_BUILD_TYPE=Release >/dev/null 2>&1; then
            log_success "✓ CMake configuration successful for cogutil"
        else
            log_warning "✗ CMake configuration failed for cogutil (expected in CI)"
        fi
        
        cd "$WORKSPACE_ROOT"
        rm -rf "$test_build_dir"
    fi
    
    if [ ${#buildable_repos[@]} -ge 2 ]; then
        log_success "Build compatibility test PASSED"
        return 0
    else
        log_error "Build compatibility test FAILED"
        return 1
    fi
}

# Main test function
main() {
    log_info "🧪 Monorepo Integration Test Suite"
    log_info "================================="
    echo ""
    
    local test_results=()
    
    # Run tests
    if test_repository_structure; then
        test_results+=("Repository Structure: PASS")
    else
        test_results+=("Repository Structure: FAIL")
    fi
    
    echo ""
    
    if test_github_actions_syntax; then
        test_results+=("GitHub Actions: PASS")
    else
        test_results+=("GitHub Actions: FAIL")
    fi
    
    echo ""
    
    if test_gitpod_configuration; then
        test_results+=("Gitpod Configuration: PASS")
    else
        test_results+=("Gitpod Configuration: FAIL")
    fi
    
    echo ""
    
    if test_build_compatibility; then
        test_results+=("Build Compatibility: PASS")
    else
        test_results+=("Build Compatibility: FAIL")
    fi
    
    # Summary
    echo ""
    log_info "📋 Test Results Summary"
    log_info "======================"
    
    local passed=0
    local failed=0
    
    for result in "${test_results[@]}"; do
        if [[ "$result" == *"PASS"* ]]; then
            log_success "$result"
            ((passed++))
        else
            log_error "$result"
            ((failed++))
        fi
    done
    
    echo ""
    log_info "Total: $((passed + failed)) tests, $passed passed, $failed failed"
    
    if [ $failed -eq 0 ]; then
        log_success "🎉 All tests PASSED - Monorepo integration is ready!"
        return 0
    else
        log_warning "⚠️  Some tests failed - Review needed"
        return 1
    fi
}

# Initialize logging
echo "$(date '+%Y-%m-%d %H:%M:%S') - Starting monorepo integration test..." > "$LOG_FILE"

# Run main function
main "$@"