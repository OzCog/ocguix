#!/bin/bash
# Test script for Shepherd deployment validation

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEST_LOG="/tmp/shepherd-deployment-test.log"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_message() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "$TEST_LOG"
}

test_success() {
    echo -e "${GREEN}✓ $1${NC}" | tee -a "$TEST_LOG"
}

test_error() {
    echo -e "${RED}✗ $1${NC}" | tee -a "$TEST_LOG"
}

test_warning() {
    echo -e "${YELLOW}⚠ $1${NC}" | tee -a "$TEST_LOG"
}

# Initialize test log
echo "=== Shepherd Deployment Test $(date) ===" > "$TEST_LOG"

test_devcontainer_structure() {
    log_message "Testing devcontainer file structure..."
    
    local required_files=(
        ".devcontainer/Dockerfile"
        ".devcontainer/devcontainer.json"
        ".devcontainer/setup-environment.sh"
        ".config/shepherd/init.scm"
        "opencog.scm"
        "shepherd-manager.sh"
    )
    
    local missing_files=0
    
    for file in "${required_files[@]}"; do
        if [ -f "$SCRIPT_DIR/$file" ]; then
            test_success "File exists: $file"
        else
            test_error "Missing required file: $file"
            missing_files=$((missing_files + 1))
        fi
    done
    
    if [ $missing_files -eq 0 ]; then
        test_success "All required files present"
        return 0
    else
        test_error "$missing_files required files missing"
        return 1
    fi
}

test_dockerfile_content() {
    log_message "Testing Dockerfile content..."
    
    local dockerfile="$SCRIPT_DIR/.devcontainer/Dockerfile"
    
    # Check for essential components
    if grep -q "FROM debian:bookworm" "$dockerfile"; then
        test_success "Dockerfile uses Debian bookworm base"
    else
        test_error "Dockerfile missing Debian base"
    fi
    
    if grep -q "guix-install.sh" "$dockerfile"; then
        test_success "Dockerfile includes Guix installation"
    else
        test_error "Dockerfile missing Guix installation"
    fi
    
    if grep -q "shepherd" "$dockerfile" && grep -q "guile" "$dockerfile"; then
        test_success "Dockerfile includes Shepherd and Guile installation"
    else
        test_error "Dockerfile missing Shepherd/Guile installation"
    fi
    
    if grep -q "vscode" "$dockerfile"; then
        test_success "Dockerfile creates development user"
    else
        test_error "Dockerfile missing development user setup"
    fi
}

test_devcontainer_json() {
    log_message "Testing devcontainer.json configuration..."
    
    local config="$SCRIPT_DIR/.devcontainer/devcontainer.json"
    
    if grep -q "Guix Shepherd OpenCog Packaging" "$config"; then
        test_success "Devcontainer has correct name"
    else
        test_error "Devcontainer missing correct name"
    fi
    
    if grep -q "dockerfile.*Dockerfile" "$config"; then
        test_success "Devcontainer references Dockerfile"
    else
        test_error "Devcontainer not configured to use Dockerfile"
    fi
    
    if grep -q "postCreateCommand" "$config"; then
        test_success "Devcontainer has post-create command"
    else
        test_warning "Devcontainer missing post-create command"
    fi
}

test_opencog_package() {
    log_message "Testing OpenCog package definition..."
    
    local package_file="$SCRIPT_DIR/opencog.scm"
    
    if grep -q "define-module.*opencog-package" "$package_file"; then
        test_success "OpenCog package module defined"
    else
        test_error "OpenCog package module missing"
    fi
    
    if grep -q "git-checkout" "$package_file" && grep -q "opencog/opencog.git" "$package_file"; then
        test_success "OpenCog package sources OpenCog repository"
    else
        test_error "OpenCog package missing correct source"
    fi
    
    if grep -q "cmake-build-system" "$package_file"; then
        test_success "OpenCog package uses CMake build system"
    else
        test_error "OpenCog package missing CMake build system"
    fi
    
    if grep -q "cogutil" "$package_file"; then
        test_success "OpenCog package includes cogutil dependency"
    else
        test_warning "OpenCog package may be missing cogutil dependency"
    fi
}

test_shepherd_configuration() {
    log_message "Testing Shepherd service configuration..."
    
    local shepherd_config="$SCRIPT_DIR/.config/shepherd/init.scm"
    
    if grep -q "opencog-build-service" "$shepherd_config"; then
        test_success "Shepherd config includes OpenCog build service"
    else
        test_error "Shepherd config missing OpenCog build service"
    fi
    
    if grep -q "opencog-test-service" "$shepherd_config"; then
        test_success "Shepherd config includes OpenCog test service"
    else
        test_error "Shepherd config missing OpenCog test service"
    fi
    
    if grep -q "guix build" "$shepherd_config"; then
        test_success "Shepherd services use Guix for building"
    else
        test_error "Shepherd services not using Guix"
    fi
    
    if grep -q "register-services" "$shepherd_config"; then
        test_success "Shepherd services are registered"
    else
        test_error "Shepherd services not registered"
    fi
}

test_shepherd_manager() {
    log_message "Testing Shepherd manager script..."
    
    local manager_script="$SCRIPT_DIR/shepherd-manager.sh"
    
    if [ -x "$manager_script" ]; then
        test_success "Shepherd manager script is executable"
    else
        test_error "Shepherd manager script not executable"
    fi
    
    if grep -q "start.*stop.*status.*build.*test" "$manager_script"; then
        test_success "Shepherd manager has required commands"
    else
        test_error "Shepherd manager missing essential commands"
    fi
    
    if grep -q "source.*guix.*profile" "$manager_script"; then
        test_success "Shepherd manager sources Guix environment"
    else
        test_error "Shepherd manager missing Guix environment setup"
    fi
}

test_integration() {
    log_message "Testing integration with existing repository..."
    
    # Check that existing guix.scm is not overwritten
    if [ -f "$SCRIPT_DIR/guix.scm" ] && grep -q "ocguix-package" "$SCRIPT_DIR/guix.scm"; then
        test_success "Existing guix.scm preserved"
    else
        test_warning "Existing guix.scm may have been modified"
    fi
    
    # Check compatibility with existing scripts
    if [ -f "$SCRIPT_DIR/test-cogutil-vendoring.sh" ]; then
        test_success "Existing test scripts preserved"
    else
        test_warning "Some existing test scripts may be missing"
    fi
}

# Main test execution
main() {
    log_message "Starting Shepherd deployment validation tests..."
    
    local total_tests=0
    local failed_tests=0
    
    # Run all tests
    test_devcontainer_structure || failed_tests=$((failed_tests + 1))
    total_tests=$((total_tests + 1))
    
    test_dockerfile_content || failed_tests=$((failed_tests + 1))
    total_tests=$((total_tests + 1))
    
    test_devcontainer_json || failed_tests=$((failed_tests + 1))
    total_tests=$((total_tests + 1))
    
    test_opencog_package || failed_tests=$((failed_tests + 1))
    total_tests=$((total_tests + 1))
    
    test_shepherd_configuration || failed_tests=$((failed_tests + 1))
    total_tests=$((total_tests + 1))
    
    test_shepherd_manager || failed_tests=$((failed_tests + 1))
    total_tests=$((total_tests + 1))
    
    test_integration || failed_tests=$((failed_tests + 1))
    total_tests=$((total_tests + 1))
    
    # Summary
    log_message "=== Test Summary ==="
    log_message "Total tests: $total_tests"
    log_message "Failed tests: $failed_tests"
    log_message "Passed tests: $((total_tests - failed_tests))"
    
    if [ $failed_tests -eq 0 ]; then
        test_success "All tests passed! Shepherd deployment is ready."
        echo ""
        echo "Next steps:"
        echo "1. Open the repository in a devcontainer-compatible IDE"
        echo "2. Run: ./shepherd-manager.sh start"
        echo "3. Run: ./shepherd-manager.sh build"
        echo "4. Check build logs: tail -f /tmp/opencog-build.log"
        return 0
    else
        test_error "Some tests failed. Please review the log: $TEST_LOG"
        return 1
    fi
}

# Handle command line arguments
case "${1:-test}" in
    test)
        main
        ;;
    help|--help|-h)
        echo "Usage: $0 [test|help]"
        echo ""
        echo "Commands:"
        echo "  test  - Run all validation tests (default)"
        echo "  help  - Show this help message"
        ;;
    *)
        echo "Unknown command: $1"
        echo "Use '$0 help' for usage information"
        exit 1
        ;;
esac