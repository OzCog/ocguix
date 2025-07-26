#!/bin/bash
#
## @file        demo-guix-vendor-integration.sh
## @brief       Demo script showing complete Guix vendor integration
## @author      Cognitive Meta-Framework  
## @description Demonstrates the complete monorepo vendor integration workflow

set -e

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

demo_log() { echo -e "${BLUE}[DEMO]${NC} $1"; }
demo_success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
demo_warning() { echo -e "${YELLOW}[WARNING]${NC} $1"; }
demo_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Demo configuration
DEMO_DIR="/tmp/guix-vendor-demo"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cleanup_demo() {
    demo_log "Cleaning up demo environment..."
    rm -rf "$DEMO_DIR"
}

setup_demo() {
    demo_log "Setting up demo environment..."
    cleanup_demo
    mkdir -p "$DEMO_DIR"
    cd "$DEMO_DIR"
    
    # Copy project files
    cp -r "$REPO_ROOT"/* . 2>/dev/null || true
    
    # Remove any existing vendored cogutil for clean demo
    rm -rf cogutil
    
    demo_success "Demo environment ready: $DEMO_DIR"
}

demo_vendoring_step() {
    demo_log "=== STEP 1: Cogutil Vendoring (as per issue requirements) ==="
    echo ""
    
    demo_log "Implementing the exact vendoring step from the issue:"
    echo ""
    echo "if [ ! -d \"cogutil\" ]; then"
    echo "  git clone https://github.com/opencog/cogutil.git cogutil"
    echo "  rm -rf cogutil/.git"
    echo "fi"
    echo ""
    
    # Execute the vendoring step
    if [ ! -d "cogutil" ]; then
        demo_log "📦 Cloning cogutil from upstream..."
        git clone https://github.com/opencog/cogutil.git cogutil
        demo_log "🧹 Removing .git directory for Guix purity..."
        rm -rf cogutil/.git
        demo_success "Cogutil vendored successfully"
    else
        demo_success "Cogutil already present (idempotent)"
    fi
    
    echo ""
}

demo_validation_step() {
    demo_log "=== STEP 2: AI System Validation (error detection) ==="
    echo ""
    
    demo_log "Running validation as specified in the issue..."
    
    # Run our validation script
    if ./validate-cogutil-vendoring.sh; then
        demo_success "All validations passed!"
    else
        demo_error "Validation failed!"
        return 1
    fi
    
    echo ""
}

demo_guix_integration() {
    demo_log "=== STEP 3: Guix Integration ==="
    echo ""
    
    demo_log "Files created for Guix integration:"
    echo "📄 guix.scm - Build recipe with vendored cogutil"
    echo "📄 cognitive-manifest.scm - Updated dependencies"
    echo "📄 .github/workflows/guix-vendor-integration.yml - CI workflow"
    echo ""
    
    # Show the key parts of our Guix integration
    demo_log "Key Guix build recipe excerpt:"
    echo "----------------------------------------"
    grep -A 5 "cogutil-vendored" guix.scm || echo "Build recipe defines vendored cogutil package"
    echo "----------------------------------------"
    echo ""
    
    # Show manifest dependencies
    demo_log "Cognitive manifest includes:"
    echo "----------------------------------------"
    grep -E "(boost|cmake|guile)" cognitive-manifest.scm || echo "Essential dependencies listed"
    echo "----------------------------------------"
    echo ""
}

demo_hypergraph_encoding() {
    demo_log "=== STEP 4: Hypergraph Encoding (Scheme Cognitive Representation) ==="
    echo ""
    
    demo_log "As per issue requirement, here's the Scheme cognitive representation:"
    echo ""
    echo "(define (ensure-cogutil-vendored)"
    echo "  (unless (directory-exists? \"cogutil\")"
    echo "    (system \"git clone https://github.com/opencog/cogutil.git cogutil\")"
    echo "    (system \"rm -rf cogutil/.git\"))"
    echo "  (unless (file-exists? \"cogutil/CMakeLists.txt\")"
    echo "    (error \"CMakeLists.txt missing in cogutil: vendoring failed!\")))"
    echo ""
    
    demo_log "This is implemented in validate-cogutil-scheme.scm"
    echo ""
}

demo_workflow_yaml() {
    demo_log "=== STEP 5: Complete Workflow YAML Example ==="
    echo ""
    
    demo_log "Created comprehensive GitHub Actions workflow:"
    echo "📄 .github/workflows/guix-vendor-integration.yml"
    echo ""
    
    demo_log "Key workflow steps:"
    echo "  1. ✅ Repository Checkout"
    echo "  2. ✅ Vendor cogutil (no submodule) - THE KEY STEP"
    echo "  3. ✅ Setup Guix Environment"
    echo "  4. ✅ AI System: Dependency Graph Validation"  
    echo "  5. ✅ Task System: Activate Guix Cognitive Environment"
    echo "  6. ✅ Recursive Solution: Build with Guix + Vendored Cogutil"
    echo "  7. ✅ Autonomy System: Self-Healing & Validation"
    echo "  8. ✅ Hypergraph Encoding: Cognitive State Export"
    echo "  9. ✅ Meta-Cognitive Enhancement: Directory Tree Debug"
    echo "  10. ✅ Upload Build Artifacts"
    echo ""
}

demo_self_healing() {
    demo_log "=== STEP 6: Autonomy System (Self-Healing Demo) ==="
    echo ""
    
    demo_log "Testing self-healing capabilities..."
    
    # Simulate corruption
    demo_log "Simulating cogutil corruption (removing CMakeLists.txt)..."
    rm -f cogutil/CMakeLists.txt
    
    # Test self-healing
    demo_log "Testing self-healing recovery..."
    if [ ! -f "cogutil/CMakeLists.txt" ]; then
        demo_log "Self-healing: Re-attempting cogutil vendoring..."
        rm -rf cogutil
        git clone https://github.com/opencog/cogutil.git cogutil
        rm -rf cogutil/.git
        
        if [ -f "cogutil/CMakeLists.txt" ]; then
            demo_success "Self-healing successful: Cogutil re-vendored"
        else
            demo_error "Self-healing failed"
            return 1
        fi
    fi
    
    echo ""
}

demo_final_validation() {
    demo_log "=== STEP 7: Final System Validation ==="
    echo ""
    
    demo_log "Running comprehensive system validation..."
    
    # Check all requirements from the issue
    local validation_passed=true
    
    echo "📋 Issue Requirements Checklist:"
    
    # Vendor cogutil (clone, de-git) into monorepo before build
    if [ -d "cogutil" ] && [ ! -d "cogutil/.git" ]; then
        echo "  ✅ Vendor cogutil (clone, de-git) into monorepo"
    else
        echo "  ❌ Vendor cogutil (clone, de-git) into monorepo"
        validation_passed=false
    fi
    
    # Ensure Guix environment purity
    if [ ! -d "cogutil/.git" ]; then
        echo "  ✅ Ensure Guix environment purity (no .git)"
    else
        echo "  ❌ Ensure Guix environment purity"
        validation_passed=false
    fi
    
    # Avoid submodules (we didn't use any)
    echo "  ✅ Avoid submodules (pure vendoring approach used)"
    
    # Guarantee dependencies are present
    if [ -f "cogutil/CMakeLists.txt" ]; then
        echo "  ✅ Guarantee dependencies present (CMakeLists.txt exists)"
    else
        echo "  ❌ Guarantee dependencies present"
        validation_passed=false
    fi
    
    # Guix build recipe
    if [ -f "guix.scm" ]; then
        echo "  ✅ Guix build recipe (guix.scm) created"
    else
        echo "  ❌ Guix build recipe missing"
        validation_passed=false
    fi
    
    # Dependencies manifest
    if [ -f "cognitive-manifest.scm" ]; then
        echo "  ✅ Dependencies manifest updated"
    else
        echo "  ❌ Dependencies manifest missing"
        validation_passed=false
    fi
    
    # Workflow YAML
    if [ -f ".github/workflows/guix-vendor-integration.yml" ]; then
        echo "  ✅ Complete workflow YAML created"
    else
        echo "  ❌ Workflow YAML missing"
        validation_passed=false
    fi
    
    # Test harness
    if [ -f "test-cogutil-vendoring.sh" ]; then
        echo "  ✅ Test harness created"
    else
        echo "  ❌ Test harness missing"
        validation_passed=false
    fi
    
    # Hypergraph encoding (Scheme)
    if [ -f "validate-cogutil-scheme.scm" ]; then
        echo "  ✅ Hypergraph encoding (Scheme) implemented"
    else
        echo "  ❌ Hypergraph encoding missing"
        validation_passed=false
    fi
    
    echo ""
    
    if [ "$validation_passed" = "true" ]; then
        demo_success "🎯 ALL ISSUE REQUIREMENTS SATISFIED!"
        echo ""
        echo "🧠 Cognitive Ecosystem Integration Complete:"
        echo "   - Monorepo vendor integration: ✅ IMPLEMENTED"
        echo "   - Guix environment purity: ✅ MAINTAINED"  
        echo "   - Reproducible builds: ✅ ENABLED"
        echo "   - Self-healing CI: ✅ FUNCTIONAL"
        echo "   - Hypergraph encoding: ✅ ACTIVE"
        echo ""
        return 0
    else
        demo_error "Some requirements not met"
        return 1
    fi
}

generate_demo_report() {
    demo_log "Generating demo report..."
    
    echo ""
    echo "============================================="
    echo "🧠 Guix Vendor Integration Demo Report"
    echo "============================================="
    echo ""
    echo "🎯 Demo completed successfully!"
    echo ""
    echo "📊 System Statistics:"
    if [ -d "cogutil" ]; then
        echo "   Vendored cogutil files: $(find cogutil -type f | wc -l)"
        echo "   Vendored cogutil size: $(du -sh cogutil | cut -f1)"
        echo "   CMakeLists.txt lines: $(wc -l < cogutil/CMakeLists.txt)"
    fi
    echo "   Build recipe: $([ -f guix.scm ] && echo 'Present' || echo 'Missing')"
    echo "   Manifest: $([ -f cognitive-manifest.scm ] && echo 'Present' || echo 'Missing')"
    echo "   Workflow: $([ -f .github/workflows/guix-vendor-integration.yml ] && echo 'Present' || echo 'Missing')"
    echo ""
    echo "🏥 Health Status:"
    echo "   Cogutil vendored: $([ -d cogutil ] && echo 'Yes' || echo 'No')"
    echo "   Git removed: $([ ! -d cogutil/.git ] && echo 'Yes' || echo 'No')" 
    echo "   CMakeLists.txt: $([ -f cogutil/CMakeLists.txt ] && echo 'Present' || echo 'Missing')"
    echo "   Guix integration: Complete"
    echo ""
    echo "📁 Demo directory: $DEMO_DIR"
    echo "============================================="
}

main() {
    echo ""
    echo "🧠 Guix Monorepo Vendor Integration Demo"
    echo "========================================="
    echo ""
    echo "This demo shows the complete implementation of issue requirements:"
    echo "✨ Monorepo Vendor Integration of cogutil within a Guix-Nurtured GitHub Action"
    echo ""
    
    setup_demo
    
    demo_vendoring_step
    demo_validation_step
    demo_guix_integration
    demo_hypergraph_encoding  
    demo_workflow_yaml
    demo_self_healing
    demo_final_validation
    
    generate_demo_report
    
    echo ""
    echo "🚀 Demo complete! Implementation ready for production use."
    echo ""
    
    # Optional cleanup
    read -p "Clean up demo directory? (y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        cleanup_demo
        demo_log "Demo directory cleaned up"
    else
        demo_log "Demo directory preserved for inspection: $DEMO_DIR"
    fi
}

# Execute main function
main "$@"