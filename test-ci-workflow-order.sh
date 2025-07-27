#!/bin/bash
# Test script to verify the correct order of steps in ci-org-gen-3.yml
# This test validates that the workflow steps are in the correct logical order

set -e

echo "🧪 Testing CI Workflow Step Order"
echo "=================================="

WORKFLOW_FILE=".github/workflows/ci-org-gen-3.yml"

if [ ! -f "$WORKFLOW_FILE" ]; then
    echo "❌ Workflow file not found: $WORKFLOW_FILE"
    exit 1
fi

echo "✅ Found workflow file: $WORKFLOW_FILE"

# Extract step names and their order from the YAML file
# We'll look for the "name:" fields under steps to get the order
STEP_ORDER=$(grep -A 1 "^      - name:" "$WORKFLOW_FILE" | grep "name:" | sed 's/.*name: //' | sed 's/^"//' | sed 's/"$//')

echo ""
echo "📋 Current step order:"
step_num=1
while IFS= read -r step; do
    echo "  $step_num. $step"
    step_num=$((step_num + 1))
done <<< "$STEP_ORDER"

echo ""
echo "🔍 Analyzing step order logic..."

# Check critical order requirements
checkout_step=$(echo "$STEP_ORDER" | grep -n "Checkout Repository" | cut -d: -f1)
install_deps_step=$(echo "$STEP_ORDER" | grep -n "Install Build Dependencies" | cut -d: -f1)
validate_step=$(echo "$STEP_ORDER" | grep -n "Validate Guix Integration" | cut -d: -f1)
clone_repos_step=$(echo "$STEP_ORDER" | grep -n "Clone Missing OpenCog Repositories" | cut -d: -f1)
setup_guix_step=$(echo "$STEP_ORDER" | grep -n "Setup Guix Environment" | cut -d: -f1)

echo "📍 Key step positions:"
echo "  - Checkout Repository: step $checkout_step"
echo "  - Setup Guix Environment: step $setup_guix_step"
echo "  - Install Build Dependencies: step $install_deps_step"
echo "  - Validate Guix Integration: step $validate_step"
echo "  - Clone Missing Repositories: step $clone_repos_step"

# Validate order requirements
errors=0

echo ""
echo "🔍 Validating step order requirements..."

# 1. Repository checkout should be FIRST (GitHub Actions convention)
if [ "$checkout_step" -ne 1 ]; then
    echo "❌ ERROR: Repository checkout should be step 1 (currently step $checkout_step)"
    echo "   GitHub Actions convention is to checkout repository first"
    errors=$((errors + 1))
else
    echo "✅ Repository checkout is step 1 (follows GitHub Actions convention)"
fi

# 2. Repository checkout should happen before dependencies
if [ "$checkout_step" -gt "$install_deps_step" ]; then
    echo "❌ ERROR: Repository checkout (step $checkout_step) happens after dependency installation (step $install_deps_step)"
    echo "   This prevents access to cognitive-manifest.scm during dependency installation"
    errors=$((errors + 1))
else
    echo "✅ Repository checkout happens before dependency installation"
fi

# 3. Repository checkout should happen before validation
if [ "$checkout_step" -gt "$validate_step" ]; then
    echo "❌ ERROR: Repository checkout (step $checkout_step) happens after validation (step $validate_step)"
    echo "   This prevents validation from accessing project files"
    errors=$((errors + 1))
else
    echo "✅ Repository checkout happens before validation"
fi

# 4. Repository checkout should happen before cloning missing repos
if [ "$checkout_step" -gt "$clone_repos_step" ]; then
    echo "❌ ERROR: Repository checkout (step $checkout_step) happens after cloning missing repos (step $clone_repos_step)"
    echo "   The main repository should be available first"
    errors=$((errors + 1))
else
    echo "✅ Repository checkout happens before cloning missing repositories"
fi

# 5. Dependency installation should happen before validation
if [ "$install_deps_step" -gt "$validate_step" ]; then
    echo "❌ ERROR: Dependency installation (step $install_deps_step) happens after validation (step $validate_step)"
    echo "   Dependencies should be available for validation"
    errors=$((errors + 1))
else
    echo "✅ Dependency installation happens before validation"
fi

echo ""
if [ $errors -eq 0 ]; then
    echo "✅ All step order requirements are satisfied!"
    exit 0
else
    echo "❌ Found $errors step order issues that need to be fixed"
    echo ""
    echo "📝 Recommended order:"
    echo "  1. Checkout Repository"
    echo "  2. Setup Guix Environment"
    echo "  3. Ensure tar is installed"
    echo "  4. Setup Local PostgreSQL with Root Access"
    echo "  5. Clone Missing OpenCog Repositories"
    echo "  6. Install Build Dependencies"
    echo "  7. Validate Guix Integration and Repository Structure"
    echo "  8. [Build steps...]"
    exit 1
fi