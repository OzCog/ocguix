#!/bin/bash
# Test script for OpenCog Theme modifications for agent interfaces

echo "🎨 Testing OpenCog Theme Integration for Agent Interfaces"
echo "======================================================="

# Change to webui directory
cd /home/runner/work/ocguix/ocguix/repos/koboldcpp/tools/server/webui

# Test 1: Build verification with themes
echo "📦 Test 1: Build verification with OpenCog themes"
if npm run build > /dev/null 2>&1; then
    echo "✅ Build successful with OpenCog themes"
else
    echo "❌ Build failed with theme modifications"
    exit 1
fi

# Test 2: Theme configuration verification
echo "🎨 Test 2: OpenCog theme configuration"
if grep -q "opencog-neural" tailwind.config.js; then
    echo "✅ OpenCog Neural theme found in configuration"
else
    echo "❌ OpenCog Neural theme missing"
    exit 1
fi

if grep -q "opencog-quantum" tailwind.config.js; then
    echo "✅ OpenCog Quantum theme found in configuration"
else
    echo "❌ OpenCog Quantum theme missing"
    exit 1
fi

# Test 3: Theme colors verification
echo "🌈 Test 3: OpenCog theme color definitions"
if grep -q "#4c6ef5" tailwind.config.js; then
    echo "✅ Neural Blue primary color defined"
else
    echo "❌ Neural Blue primary color missing"
    exit 1
fi

if grep -q "#51cf66" tailwind.config.js; then
    echo "✅ Synapse Green secondary color defined"
else
    echo "❌ Synapse Green secondary color missing"
    exit 1
fi

if grep -q "#9775fa" tailwind.config.js; then
    echo "✅ Cognition Purple accent color defined"
else
    echo "❌ Cognition Purple accent color missing"
    exit 1
fi

# Test 4: Theme export in Config.ts
echo "⚙️ Test 4: Theme export configuration"
if grep -q "opencog-neural" src/Config.ts; then
    echo "✅ OpenCog Neural theme exported in Config.ts"
else
    echo "❌ OpenCog Neural theme not exported"
    exit 1
fi

if grep -q "opencog-quantum" src/Config.ts; then
    echo "✅ OpenCog Quantum theme exported in Config.ts"
else
    echo "❌ OpenCog Quantum theme not exported"
    exit 1
fi

# Test 5: Default theme setting
echo "🔧 Test 5: Default theme configuration"
if grep -q "opencog-neural" src/utils/storage.ts; then
    echo "✅ Default theme set to OpenCog Neural"
else
    echo "❌ Default theme not set to OpenCog Neural"
    exit 1
fi

# Test 6: CSS enhancements verification
echo "✨ Test 6: OpenCog CSS enhancements"
if grep -q "neural-pulse" src/index.scss; then
    echo "✅ Neural pulse animation defined"
else
    echo "❌ Neural pulse animation missing"
    exit 1
fi

if grep -q "quantum-glow" src/index.scss; then
    echo "✅ Quantum glow animation defined"
else
    echo "❌ Quantum glow animation missing"
    exit 1
fi

if grep -q "agent-card" src/index.scss; then
    echo "✅ Agent card styling enhancements found"
else
    echo "❌ Agent card styling enhancements missing"
    exit 1
fi

# Test 7: Component integration
echo "🧩 Test 7: Component integration with themes"
if grep -q "agent-card" src/components/dashboards/SKZDashboard.tsx; then
    echo "✅ Agent card class applied to dashboard components"
else
    echo "❌ Agent card class not applied to dashboard components"
    exit 1
fi

# Test 8: Header theme tooltip update
echo "🎯 Test 8: Header theme selector updates"
if grep -q "OpenCog Agent Themes" src/components/Header.tsx; then
    echo "✅ Header theme tooltip updated for OpenCog themes"
else
    echo "❌ Header theme tooltip not updated"
    exit 1
fi

echo ""
echo "🎉 All tests passed! OpenCog theme modifications implemented successfully."
echo ""
echo "📋 Theme Integration Summary:"
echo "   ✅ OpenCog Neural theme (light cognitive interface)"
echo "   ✅ OpenCog Quantum theme (dark quantum-inspired interface)"
echo "   ✅ Custom color palette (Neural Blue, Synapse Green, Cognition Purple)"
echo "   ✅ Enhanced animations (neural-pulse, quantum-glow)"
echo "   ✅ Agent card enhancements with hover effects"
echo "   ✅ Default OpenCog Neural theme for new users"
echo "   ✅ Theme selector integration in header"
echo "   ✅ Compatible with existing agent dashboard components"
echo "   ✅ Build system integration successful"
echo ""
echo "🧠 OpenCog Agent Interface Theming Complete!"
echo "   Themes provide distinct cognitive/AI visual identity"
echo "   Enhanced user experience for agent interactions"
echo "   Seamless integration with existing infrastructure"