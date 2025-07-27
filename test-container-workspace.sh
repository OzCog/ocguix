#!/bin/bash
#
# test-container-workspace.sh - Comprehensive test for container workspace functionality
# Tests the exact scenarios that could cause "container workspace ran with an error: exit code 1"
#

set -e

echo "🧪 Container Workspace Comprehensive Test"
echo "========================================"
echo ""

TESTS_PASSED=0
TESTS_TOTAL=0

# Test function
run_test() {
    local test_name="$1"
    local test_command="$2"
    
    TESTS_TOTAL=$((TESTS_TOTAL + 1))
    echo "🔍 Test $TESTS_TOTAL: $test_name"
    
    if eval "$test_command"; then
        echo "✅ PASSED: $test_name"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo "❌ FAILED: $test_name"
        echo "   Command: $test_command"
    fi
    echo ""
}

# Test 1: Registry Discovery Agent
run_test "Registry Discovery Agent" "guile ./registry-discovery-agent.scm >/dev/null 2>&1"

# Test 2: Profile Extraction Agent  
run_test "Profile Extraction Agent" "guile ./profile-extraction-agent.scm >/dev/null 2>&1"

# Test 3: Artifact Synthesis Agent
run_test "Artifact Synthesis Agent" "guile ./artifact-synthesis-agent.scm >/dev/null 2>&1"

# Test 4: Meta-Cognitive Feedback Agent
run_test "Meta-Cognitive Feedback Agent" "guile ./meta-cognitive-feedback-agent.scm >/dev/null 2>&1"

# Test 5: Output Files Generation
run_test "Required Output Files Exist" 'test -f "/tmp/registry_listing.json" && test -f "/tmp/build_profiles_scan.json" && test -f "/tmp/artifact_synthesis.json" && test -f "/tmp/cognitive_health_metrics.json"'

# Test 6: CI Workflow File Checks (simulating the actual CI logic)
run_test "CI Registry File Check" 'if [ -f "/tmp/registry_listing.json" ]; then echo "✅ Registry discovery completed successfully" >/dev/null; else echo "❌ Registry discovery failed - output file not found" >&2; exit 1; fi'

run_test "CI Profile File Check" 'if [ -f "/tmp/build_profiles_scan.json" ]; then echo "✅ Profile extraction completed successfully" >/dev/null; else echo "❌ Profile extraction failed - output file not found" >&2; exit 1; fi'

run_test "CI Artifact File Check" 'if [ -f "/tmp/artifact_synthesis.json" ]; then echo "✅ Artifact synthesis completed successfully" >/dev/null; else echo "❌ Artifact synthesis failed - output file not found" >&2; exit 1; fi'

run_test "CI Cognitive File Check" 'if [ -f "/tmp/cognitive_health_metrics.json" ]; then echo "✅ Meta-cognitive analysis completed successfully" >/dev/null; else echo "❌ Meta-cognitive analysis failed - output file not found" >&2; exit 1; fi'

# Test 7: Test Script Execution
run_test "Cognitive Flowchart Test Script" "./test-cognitive-flowchart.sh >/dev/null 2>&1"

# Test 8: Network Offline Mode Handling
run_test "Offline Mode Detection" 'guile ./registry-discovery-agent.scm 2>&1 | grep -q "Offline environment detected"'

# Test 9: No Alarming Error Messages in Output
run_test "No Alarming Error Messages" '! (guile ./registry-discovery-agent.scm 2>&1 | grep -q "❌")'

# Test 10: Setup Script
run_test "Gitpod Setup Script" "./.gitpod/setup.sh >/dev/null 2>&1"

echo "📊 Test Results Summary"
echo "======================"
echo "✅ Tests Passed: $TESTS_PASSED"
echo "📊 Tests Total:  $TESTS_TOTAL"

if [ $TESTS_PASSED -eq $TESTS_TOTAL ]; then
    echo ""
    echo "🎉 ALL TESTS PASSED!"
    echo "🔧 Container workspace error has been resolved"
    echo "✅ All cognitive agents work correctly in container environments"
    echo "ℹ️  Offline mode handling implemented successfully"
    echo "🚀 System ready for CI/CD deployment"
    exit 0
else
    echo ""
    echo "⚠️  Some tests failed ($((TESTS_TOTAL - TESTS_PASSED)) failures)"
    echo "❌ Container workspace issues may still exist"
    exit 1
fi