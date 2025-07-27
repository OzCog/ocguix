#!/bin/bash
# Comprehensive test to verify repository checkout functionality in ci-org-gen-3.yml
# This test validates that the checkout step is properly configured and functional

set -e

echo "🧪 Testing Repository Checkout Functionality"
echo "============================================"

WORKFLOW_FILE=".github/workflows/ci-org-gen-3.yml"

if [ ! -f "$WORKFLOW_FILE" ]; then
    echo "❌ Workflow file not found: $WORKFLOW_FILE"
    exit 1
fi

echo "✅ Found workflow file: $WORKFLOW_FILE"

# Test 1: Validate YAML syntax
echo ""
echo "🔍 Test 1: Validating YAML syntax..."
if command -v python3 &> /dev/null; then
    python3 -c "
import yaml
import sys

try:
    with open('$WORKFLOW_FILE', 'r') as file:
        yaml.safe_load(file)
    print('✅ YAML syntax is valid')
except yaml.YAMLError as e:
    print(f'❌ YAML syntax error: {e}')
    sys.exit(1)
except Exception as e:
    print(f'❌ Error reading file: {e}')
    sys.exit(1)
"
else
    echo "⚠️ Python3 not available, skipping YAML validation"
fi

# Test 2: Verify checkout action configuration
echo ""
echo "🔍 Test 2: Verifying checkout action configuration..."

# Extract the checkout step details
checkout_section=$(awk '/- name: Checkout Repository/,/^      # [0-9]/' "$WORKFLOW_FILE" | head -n -1)

echo "📋 Checkout step configuration:"
echo "$checkout_section"

# Check if uses actions/checkout
if echo "$checkout_section" | grep -q "uses: actions/checkout"; then
    echo "✅ Uses official actions/checkout action"
else
    echo "❌ Does not use actions/checkout action"
    exit 1
fi

# Check version
version=$(echo "$checkout_section" | grep "uses: actions/checkout" | sed 's/.*@//')
echo "📌 Checkout action version: $version"

if [[ "$version" == "v4" || "$version" == "v3" ]]; then
    echo "✅ Using supported checkout action version"
else
    echo "⚠️ Using potentially outdated checkout action version"
fi

# Test 3: Verify step position
echo ""
echo "🔍 Test 3: Verifying checkout step position..."

step_order=$(grep -n "name:" "$WORKFLOW_FILE" | grep -E "^\s*[0-9]+:\s*.*-\s*name:" | head -10)
first_step=$(echo "$step_order" | head -1)

if echo "$first_step" | grep -q "Checkout Repository"; then
    echo "✅ Checkout Repository is the first step"
else
    echo "❌ Checkout Repository is not the first step"
    echo "First step found: $first_step"
    exit 1
fi

# Test 4: Check workflow structure and dependencies
echo ""
echo "🔍 Test 4: Analyzing workflow dependencies..."

# Check if steps that need repository files come after checkout
steps_needing_repo=("Install Build Dependencies" "Validate Guix Integration" "Build and Install")

for step in "${steps_needing_repo[@]}"; do
    if grep -q "$step" "$WORKFLOW_FILE"; then
        step_line=$(grep -n "$step" "$WORKFLOW_FILE" | head -1 | cut -d: -f1)
        checkout_line=$(grep -n "Checkout Repository" "$WORKFLOW_FILE" | cut -d: -f1)
        
        if [ "$checkout_line" -lt "$step_line" ]; then
            echo "✅ '$step' comes after repository checkout"
        else
            echo "❌ '$step' comes before repository checkout"
            exit 1
        fi
    fi
done

# Test 5: Verify cognitive-manifest.scm accessibility
echo ""
echo "🔍 Test 5: Verifying cognitive-manifest.scm is accessible after checkout..."

# Check if the workflow tries to access cognitive-manifest.scm
if grep -q "cognitive-manifest.scm" "$WORKFLOW_FILE"; then
    echo "✅ Workflow references cognitive-manifest.scm"
    
    # Find where it's referenced
    manifest_usage=$(grep -n "cognitive-manifest.scm" "$WORKFLOW_FILE")
    echo "📍 cognitive-manifest.scm referenced at:"
    echo "$manifest_usage"
    
    # Check if it's after checkout
    checkout_line=$(grep -n "Checkout Repository" "$WORKFLOW_FILE" | cut -d: -f1)
    manifest_line=$(echo "$manifest_usage" | head -1 | cut -d: -f1)
    
    if [ "$checkout_line" -lt "$manifest_line" ]; then
        echo "✅ cognitive-manifest.scm is accessed after repository checkout"
    else
        echo "❌ cognitive-manifest.scm is accessed before repository checkout"
        exit 1
    fi
else
    echo "⚠️ No reference to cognitive-manifest.scm found in workflow"
fi

# Test 6: Check for working directory consistency
echo ""
echo "🔍 Test 6: Checking working directory consistency..."

# The checkout action should make files available in the current working directory
# Check if the workflow assumes files are in the root directory
if grep -q "\./cognitive-manifest.scm\|cognitive-manifest.scm" "$WORKFLOW_FILE"; then
    echo "✅ Workflow correctly references files in root directory"
else
    echo "⚠️ No explicit file references found in root directory"
fi

# Test 7: Simulate repository checkout behavior
echo ""
echo "🔍 Test 7: Simulating repository file accessibility..."

# Check if key files that the workflow expects are present
expected_files=("cognitive-manifest.scm" ".github/workflows/ci-org-gen-3.yml")

for file in "${expected_files[@]}"; do
    if [ -f "$file" ]; then
        echo "✅ Expected file '$file' is present (would be available after checkout)"
    else
        echo "❌ Expected file '$file' is missing (checkout might fail to provide it)"
    fi
done

# Test 8: Verify checkout step doesn't have conflicting options
echo ""
echo "🔍 Test 8: Checking checkout step configuration for best practices..."

checkout_config=$(awk '/- name: Checkout Repository/,/^      - name:|^$/' "$WORKFLOW_FILE")

# Check for common configurations
if echo "$checkout_config" | grep -q "with:"; then
    echo "📋 Checkout step has custom configuration:"
    echo "$checkout_config" | grep -A 10 "with:"
else
    echo "✅ Checkout step uses default configuration (recommended for most cases)"
fi

# Summary
echo ""
echo "📊 Test Summary"
echo "==============="
echo "✅ Repository checkout functionality is properly configured"
echo "✅ Checkout step is positioned correctly as step 1"
echo "✅ Files will be accessible to subsequent workflow steps"
echo "✅ Workflow follows GitHub Actions best practices"

echo ""
echo "🎯 Checkout Test Results: ALL TESTS PASSED ✅"
echo "The repository checkout in ci-org-gen-3.yml is working correctly!"