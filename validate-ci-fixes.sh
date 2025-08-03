#!/bin/bash

# CI Build Fixes Validation Script
# This script validates that the fixes for CI build failures are working correctly

set -e

echo "🔍 Validating CI Build Fixes..."
echo "=================================="

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    local status=$1
    local message=$2
    if [ "$status" = "PASS" ]; then
        echo -e "${GREEN}✅ PASS${NC}: $message"
    elif [ "$status" = "FAIL" ]; then
        echo -e "${RED}❌ FAIL${NC}: $message"
    else
        echo -e "${YELLOW}⚠️  WARN${NC}: $message"
    fi
}

# Test 1: Check if git safe.directory is configured
echo ""
echo "1. Testing Git Safe Directory Configuration..."
if git config --global --get-all safe.directory | grep -q "$(pwd)"; then
    print_status "PASS" "Git safe.directory configured for current directory"
else
    print_status "WARN" "Git safe.directory not configured for current directory"
    echo "   To fix: git config --global --add safe.directory $(pwd)"
fi

# Test 2: Check if valgrind is available
echo ""
echo "2. Testing Valgrind Availability..."
if command -v valgrind >/dev/null 2>&1; then
    print_status "PASS" "Valgrind is installed: $(valgrind --version | head -n1)"
else
    print_status "FAIL" "Valgrind is not installed"
    echo "   To fix: sudo apt-get update && sudo apt-get install -y valgrind"
fi

# Test 3: Check if the missing include file exists
echo ""
echo "3. Testing Missing Include File..."
if [ -f "repos/cogutil/opencog/util/iostreamContainer.h" ]; then
    print_status "PASS" "iostreamContainer.h exists"
else
    print_status "FAIL" "iostreamContainer.h not found"
fi

# Test 4: Check if the include was added to table.h
echo ""
echo "4. Testing Include Statement in table.h..."
if grep -q "#include <opencog/util/iostreamContainer.h>" "repos/moses/moses/comboreduct/table/table.h"; then
    print_status "PASS" "iostreamContainer.h include added to table.h"
else
    print_status "FAIL" "iostreamContainer.h include missing from table.h"
fi

# Test 5: Check workflow file for fixes
echo ""
echo "5. Testing Workflow File Fixes..."
if grep -q "git config --global --add safe.directory" ".github/workflows/oc.yml"; then
    print_status "PASS" "Git safe.directory configuration added to workflow"
else
    print_status "FAIL" "Git safe.directory configuration missing from workflow"
fi

if grep -q "valgrind" ".github/workflows/oc.yml"; then
    print_status "PASS" "Valgrind installation added to workflow"
else
    print_status "FAIL" "Valgrind installation missing from workflow"
fi

# Test 6: Check database configuration
echo ""
echo "6. Testing Database Configuration..."
if grep -q "POSTGRES_USER: opencog_test" ".github/workflows/oc.yml"; then
    print_status "PASS" "Database configured with opencog_test user"
else
    print_status "FAIL" "Database not configured with opencog_test user"
fi

# Test 7: Check if issue template exists
echo ""
echo "7. Testing Issue Template..."
if [ -f ".github/ISSUE_TEMPLATE/ci-build-failure.md" ]; then
    print_status "PASS" "CI build failure issue template created"
else
    print_status "FAIL" "CI build failure issue template missing"
fi

# Test 8: Check documentation
echo ""
echo "8. Testing Documentation..."
if [ -f "CI-BUILD-FIXES.md" ]; then
    print_status "PASS" "CI build fixes documentation created"
else
    print_status "FAIL" "CI build fixes documentation missing"
fi

# Summary
echo ""
echo "=================================="
echo "📊 Validation Summary:"
echo "=================================="

# Count passes and fails
passes=$(grep -c "✅ PASS" <<< "$(tail -n +1)")
fails=$(grep -c "❌ FAIL" <<< "$(tail -n +1)")
warns=$(grep -c "⚠️  WARN" <<< "$(tail -n +1)")

echo "✅ Passes: $passes"
echo "❌ Fails: $fails"
echo "⚠️  Warnings: $warns"

if [ "$fails" -eq 0 ]; then
    echo ""
    print_status "PASS" "All critical fixes are in place!"
    echo "   The CI build should now work correctly."
else
    echo ""
    print_status "FAIL" "Some fixes are missing. Please address the failures above."
    exit 1
fi

echo ""
echo "🚀 Next Steps:"
echo "1. Commit and push these changes"
echo "2. Trigger a CI build to verify fixes"
echo "3. Monitor the build logs for any remaining issues"
echo "4. Update documentation if new issues are found"