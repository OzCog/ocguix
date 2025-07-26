#!/bin/bash
#
## @file        validate-cogutil-vendoring.sh  
## @brief       Simple validation for cogutil vendoring (CI-friendly)
## @author      Cognitive Meta-Framework
## @description Quick validation that cogutil vendoring worked correctly

set -e

# Simple validation function
validate_cogutil() {
    echo "🔍 Validating cogutil vendoring..."
    
    # Check if cogutil directory exists
    if [ ! -d "cogutil" ]; then
        echo "❌ ERROR: cogutil directory not found!"
        return 1
    fi
    echo "✅ cogutil directory present"
    
    # Check if CMakeLists.txt exists
    if [ ! -f "cogutil/CMakeLists.txt" ]; then
        echo "❌ ERROR: cogutil/CMakeLists.txt not found!"
        return 1
    fi
    echo "✅ cogutil/CMakeLists.txt present"
    
    # Check that .git directory was removed
    if [ -d "cogutil/.git" ]; then
        echo "❌ ERROR: cogutil/.git directory found! Guix purity violated."
        return 1
    fi
    echo "✅ cogutil/.git directory properly removed"
    
    # Validate CMakeLists.txt has reasonable content
    local cmake_lines=$(wc -l < cogutil/CMakeLists.txt)
    if [ "$cmake_lines" -lt 10 ]; then
        echo "❌ ERROR: cogutil/CMakeLists.txt seems too short ($cmake_lines lines)"
        return 1
    fi
    echo "✅ cogutil/CMakeLists.txt has $cmake_lines lines (reasonable)"
    
    # Check for essential cogutil structure
    if [ ! -d "cogutil/opencog" ]; then
        echo "⚠️ WARNING: cogutil/opencog directory not found"
    else
        echo "✅ cogutil/opencog directory present"
    fi
    
    echo "🎯 Cogutil vendoring validation: PASSED"
    return 0
}

# Main execution
if [ "$1" = "--help" ] || [ "$1" = "-h" ]; then
    echo "Cogutil Vendoring Validator"
    echo "Usage: $0 [directory]"
    echo ""
    echo "Validates that cogutil has been properly vendored in the current directory"
    echo "or the specified directory."
    exit 0
fi

# Change to specified directory if provided
if [ -n "$1" ]; then
    cd "$1"
fi

echo "📂 Validating cogutil in: $(pwd)"
validate_cogutil