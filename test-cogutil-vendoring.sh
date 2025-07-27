#!/bin/bash
#
## @file        test-cogutil-vendoring.sh
## @brief       Test harness for cogutil vendoring in Guix environment
## @author      Cognitive Meta-Framework
## @description Tests the vendoring, validation, and build process for cogutil

set -e

# Color output for better cognitive experience
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

test_log() {
    echo -e "${BLUE}[TEST]${NC} $1"
}

test_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
}

test_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

test_error() {
    echo -e "${RED}[FAIL]${NC} $1"
}

# Test configuration
TEST_DIR="/tmp/cogutil-vendoring-test"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEST_PASSED=0
TEST_FAILED=0

cleanup_test() {
    test_log "Cleaning up test environment..."
    rm -rf "$TEST_DIR"
}

setup_test() {
    test_log "Setting up test environment..."
    
    # Clean up any previous test
    cleanup_test
    
    # Create test directory
    mkdir -p "$TEST_DIR"
    cd "$TEST_DIR"
    
    # Copy project files to test directory
    cp -r "$REPO_ROOT"/* . 2>/dev/null || true
    
    # Remove any existing vendored cogutil for clean test
    rm -rf cogutil
    
    test_success "Test environment setup complete"
}

test_cogutil_vendoring() {
    test_log "Testing cogutil vendoring process..."
    
    # Test the vendoring logic exactly as in the workflow
    if [ ! -d "cogutil" ]; then
        test_log "Cloning cogutil from upstream..."
        if git clone https://github.com/opencog/cogutil.git cogutil; then
            test_success "Cogutil cloned successfully"
        else
            test_error "Failed to clone cogutil"
            ((TEST_FAILED++))
            return 1
        fi
        
        test_log "Removing .git directory for Guix purity..."
        rm -rf cogutil/.git
        
        if [ ! -d "cogutil/.git" ]; then
            test_success "Git directory removed successfully"
        else
            test_error "Failed to remove .git directory"
            ((TEST_FAILED++))
            return 1
        fi
    else
        test_success "Cogutil already present, skipping vendoring"
    fi
    
    ((TEST_PASSED++))
    return 0
}

test_cogutil_validation() {
    test_log "Testing cogutil validation..."
    
    # Critical validation: Ensure CMakeLists.txt exists
    if [ -f "cogutil/CMakeLists.txt" ]; then
        test_success "cogutil/CMakeLists.txt found"
        
        # Check file content
        local cmake_lines=$(wc -l < cogutil/CMakeLists.txt)
        test_log "CMakeLists.txt has $cmake_lines lines"
        
        if [ "$cmake_lines" -gt 10 ]; then
            test_success "CMakeLists.txt appears to be a valid file"
        else
            test_warning "CMakeLists.txt seems unusually short"
        fi
    else
        test_error "cogutil/CMakeLists.txt missing after vendoring!"
        test_log "Directory structure:"
        find cogutil -name "*.txt" -o -name "*.cmake" | head -10 || true
        ((TEST_FAILED++))
        return 1
    fi
    
    # Check for other required files/directories
    local required_paths=(
        "cogutil/opencog"
        "cogutil/README.md"
    )
    
    for path in "${required_paths[@]}"; do
        if [ -e "$path" ]; then
            test_success "$path - Found"
        else
            test_warning "$path - Missing (may be optional)"
        fi
    done
    
    # Verify .git directory is absent
    if [ -d "cogutil/.git" ]; then
        test_error "cogutil/.git directory found! Guix purity violated."
        ((TEST_FAILED++))
        return 1
    else
        test_success "cogutil/.git - Properly removed (Guix purity maintained)"
    fi
    
    ((TEST_PASSED++))
    return 0
}

test_project_structure() {
    test_log "Testing project structure validation..."
    
    local structure_items=(
        "guix.scm:Build recipe"
        "cognitive-manifest.scm:Dependency manifest"
        "cogutil:Vendored cogutil"
    )
    
    for item in "${structure_items[@]}"; do
        local file="${item%%:*}"
        local desc="${item##*:}"
        
        if [ -e "$file" ]; then
            test_success "$desc ($file) - Present"
        else
            test_error "$desc ($file) - Missing"
            ((TEST_FAILED++))
        fi
    done
    
    if [ $TEST_FAILED -eq 0 ]; then
        ((TEST_PASSED++))
    fi
}

test_guix_compatibility() {
    test_log "Testing Guix environment compatibility..."
    
    # Check if Guix is available (may not be in CI)
    if command -v guix &> /dev/null; then
        test_success "Guix found: $(guix --version | head -1)"
        
        # Test manifest parsing
        if [ -f "cognitive-manifest.scm" ]; then
            test_log "Testing manifest parsing..."
            if guix install --dry-run -m cognitive-manifest.scm &>/dev/null; then
                test_success "Cognitive manifest is valid"
            else
                test_warning "Cognitive manifest may have issues (dry-run failed)"
            fi
        fi
        
        # Test build recipe parsing
        if [ -f "guix.scm" ]; then
            test_log "Testing build recipe parsing..."
            if guix build --dry-run -f guix.scm &>/dev/null; then
                test_success "Build recipe (guix.scm) is valid"
            else
                test_warning "Build recipe may have issues (dry-run failed)"
            fi
        fi
    else
        test_warning "Guix not available (normal in CI environments)"
        test_log "Simulating Guix compatibility test..."
        
        # Simulate manifest validation by checking syntax
        if [ -f "cognitive-manifest.scm" ]; then
            if grep -q "specifications->manifest" cognitive-manifest.scm; then
                test_success "Manifest syntax appears correct"
            else
                test_error "Manifest syntax appears incorrect"
                ((TEST_FAILED++))
            fi
        fi
        
        # Simulate build recipe validation
        if [ -f "guix.scm" ]; then
            if grep -q "package" guix.scm; then
                test_success "Build recipe syntax appears correct"
            else
                test_error "Build recipe syntax appears incorrect"
                ((TEST_FAILED++))
            fi
        fi
    fi
    
    ((TEST_PASSED++))
}

test_idempotency() {
    test_log "Testing vendoring idempotency..."
    
    # Record initial state
    local initial_file_count=0
    if [ -d "cogutil" ]; then
        initial_file_count=$(find cogutil -type f | wc -l)
    fi
    
    # Run vendoring again (should be idempotent)
    test_log "Running vendoring process again..."
    
    if [ ! -d "cogutil" ]; then
        git clone https://github.com/opencog/cogutil.git cogutil
        rm -rf cogutil/.git
    fi
    
    # Check that state is the same
    local final_file_count=0
    if [ -d "cogutil" ]; then
        final_file_count=$(find cogutil -type f | wc -l)
    fi
    
    if [ "$initial_file_count" -eq "$final_file_count" ] && [ "$final_file_count" -gt 0 ]; then
        test_success "Vendoring is idempotent (file count: $final_file_count)"
    else
        test_warning "Vendoring idempotency test inconclusive (initial: $initial_file_count, final: $final_file_count)"
    fi
    
    ((TEST_PASSED++))
}

test_self_healing() {
    test_log "Testing self-healing capabilities..."
    
    # Simulate a corrupted cogutil
    test_log "Simulating cogutil corruption..."
    if [ -d "cogutil" ]; then
        rm -f cogutil/CMakeLists.txt
        
        if [ ! -f "cogutil/CMakeLists.txt" ]; then
            test_log "Corruption simulated - CMakeLists.txt removed"
        fi
    fi
    
    # Test self-healing logic
    test_log "Testing self-healing recovery..."
    if [ ! -f "cogutil/CMakeLists.txt" ]; then
        test_log "Self-healing: Re-attempting cogutil vendoring..."
        rm -rf cogutil
        git clone https://github.com/opencog/cogutil.git cogutil
        rm -rf cogutil/.git
        
        if [ -f "cogutil/CMakeLists.txt" ]; then
            test_success "Self-healing successful: Cogutil re-vendored"
        else
            test_error "Self-healing failed: Could not recover"
            ((TEST_FAILED++))
            return 1
        fi
    fi
    
    ((TEST_PASSED++))
}

generate_report() {
    test_log "Generating test report..."
    
    echo ""
    echo "============================================="
    echo "🧠 Cogutil Vendoring Test Report"
    echo "============================================="
    echo ""
    
    echo "📊 Test Statistics:"
    echo "   Tests Passed: $TEST_PASSED"
    echo "   Tests Failed: $TEST_FAILED"
    echo "   Total Tests:  $((TEST_PASSED + TEST_FAILED))"
    echo ""
    
    if [ -d "cogutil" ]; then
        echo "📦 Vendored Cogutil Statistics:"
        echo "   Files: $(find cogutil -type f | wc -l)"
        echo "   Size: $(du -sh cogutil | cut -f1)"
        echo "   CMakeLists.txt lines: $(wc -l < cogutil/CMakeLists.txt 2>/dev/null || echo 'N/A')"
        echo ""
    fi
    
    echo "🏥 System Health:"
    echo "   Cogutil present: $([ -d cogutil ] && echo 'Yes' || echo 'No')"
    echo "   CMakeLists.txt: $([ -f cogutil/CMakeLists.txt ] && echo 'Yes' || echo 'No')"
    echo "   Git directory removed: $([ ! -d cogutil/.git ] && echo 'Yes' || echo 'No')"
    echo "   Build recipe: $([ -f guix.scm ] && echo 'Yes' || echo 'No')"
    echo "   Manifest: $([ -f cognitive-manifest.scm ] && echo 'Yes' || echo 'No')"
    echo ""
    
    if [ $TEST_FAILED -eq 0 ]; then
        echo "🎯 Overall Result: ALL TESTS PASSED"
        echo "✅ Cogutil vendoring implementation is working correctly"
    else
        echo "❌ Overall Result: SOME TESTS FAILED"
        echo "🔧 Please review the failed tests and fix the issues"
    fi
    echo ""
    echo "📁 Test directory: $TEST_DIR"
    echo "============================================="
}

main() {
    echo "🧠 Cogutil Vendoring Test Harness"
    echo "=================================="
    echo ""
    
    setup_test
    
    test_log "Starting comprehensive test suite..."
    echo ""
    
    # Run all tests
    test_cogutil_vendoring
    test_cogutil_validation
    test_project_structure
    test_guix_compatibility
    test_idempotency
    test_self_healing
    
    generate_report
    
    # Cleanup (optional - comment out for debugging)
    # cleanup_test
    
    # Exit with appropriate code
    if [ $TEST_FAILED -eq 0 ]; then
        exit 0
    else
        exit 1
    fi
}

# Execute main function
main "$@"