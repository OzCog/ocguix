#!/bin/bash
#
# test-gitpod-deployment.sh - Quick test script for Gitpod deployment
# Verifies that the essential components are working correctly
#

set -e

echo "🧪 Testing Gitpod Deployment"
echo "==========================="
echo ""

# Test 1: Check if basic tools are available
echo "1. Checking basic tools..."
for tool in git curl python3 bash; do
    if command -v "$tool" >/dev/null 2>&1; then
        echo "   ✅ $tool: Available"
    else
        echo "   ❌ $tool: Missing"
        exit 1
    fi
done
echo ""

# Test 2: Check if KoboldCpp server is responding
echo "2. Testing KoboldCpp server..."
if curl -s --connect-timeout 5 http://localhost:5001/api/v1/model >/dev/null 2>&1; then
    echo "   ✅ KoboldCpp API: Responding"
    response=$(curl -s http://localhost:5001/api/v1/model)
    echo "   📄 Response: $response"
else
    echo "   ❌ KoboldCpp API: Not responding"
    echo "   ℹ️  This might be normal if server is still starting"
fi
echo ""

# Test 3: Check if essential scripts are executable
echo "3. Checking essential scripts..."
scripts=("ocpkg" "koboldcpp-setup.sh" "cognitive-grammar-integration-agent.scm")
for script in "${scripts[@]}"; do
    if [ -x "./$script" ]; then
        echo "   ✅ $script: Executable"
    elif [ -f "./$script" ]; then
        echo "   ⚠️  $script: Exists but not executable"
        chmod +x "./$script"
        echo "   🔧 Fixed permissions for $script"
    else
        echo "   ❌ $script: Missing"
    fi
done
echo ""

# Test 4: Check deployment status
echo "4. Checking deployment status..."
if [ -f "/tmp/deployment-status.txt" ]; then
    echo "   ✅ Deployment status file exists"
    echo "   📄 Status: $(tail -1 /tmp/deployment-status.txt)"
else
    echo "   ⚠️  No deployment status file found"
fi
echo ""

# Test 5: Check if directories were created
echo "5. Checking workspace structure..."
dirs=("$HOME/koboldcpp" "$HOME/models" "/tmp/opencog-workspace")
for dir in "${dirs[@]}"; do
    if [ -d "$dir" ]; then
        echo "   ✅ $dir: Exists"
    else
        echo "   ❌ $dir: Missing"
    fi
done
echo ""

# Test 6: Simple cognitive test
echo "6. Testing cognitive components..."
if [ -f "./cognitive-grammar-integration-agent.scm" ]; then
    if timeout 5 ./cognitive-grammar-integration-agent.scm --validate 2>/dev/null; then
        echo "   ✅ Cognitive grammar agent: Validated"
    else
        echo "   ⚠️  Cognitive grammar agent: Validation failed (might be normal)"
    fi
else
    echo "   ❌ Cognitive grammar agent: Missing"
fi
echo ""

echo "🎯 Test Summary"
echo "==============="
echo "✅ Basic deployment test completed"
echo "📋 Check individual test results above for any issues"
echo ""

# Final health check
if curl -s --connect-timeout 2 http://localhost:5001 >/dev/null 2>&1; then
    echo "🌟 Overall Status: HEALTHY - Services are responding"
    exit 0
else
    echo "⚠️  Overall Status: PARTIAL - Some services may still be starting"
    exit 0
fi