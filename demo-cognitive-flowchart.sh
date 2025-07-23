#!/bin/bash
# Cognitive Flowchart Demo Script
# Demonstrates the integration of registry sources and build profiles

set -e

echo "🧠 Cognitive Flowchart Demo: Registry & Build Profile Management"
echo "=============================================================="

# Create temp directory for demo
DEMO_DIR="/tmp/cognitive-flowchart-demo"
mkdir -p "$DEMO_DIR"

echo "📁 Demo directory: $DEMO_DIR"
echo ""

# Simulate registry discovery
echo "🔍 Step 1: Registry Source Discovery"
echo "-----------------------------------"

if [ -f "registry-sources.scm" ]; then
    echo "✅ Found registry-sources.scm"
    echo "📡 Simulating registry discovery..."
    
    # Generate registry listing
    cat > "$DEMO_DIR/registry_listing.json" << EOF
{
  "generated": "$(date -u -Iseconds)",
  "registries": [
    {
      "id": "opencog-github",
      "url": "https://github.com/opencog/*",
      "status": "active",
      "categories": ["AGI", "cognitive-architecture", "atomspace", "reasoning"],
      "attributes": ["public", "maintained", "open-source"],
      "tensor_shape": [1, "registry_count", "url_complexity", "tag_cardinality"],
      "repos_discovered": ["atomspace", "opencog", "cogutil", "moses", "relex"]
    },
    {
      "id": "guix-packages",
      "url": "https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages",
      "status": "active",
      "categories": ["package-management", "functional", "reproducible"],
      "attributes": ["official", "curated", "immutable"],
      "tensor_shape": [1, "package_count", "complexity_level", "stability_index"]
    }
  ],
  "summary": {
    "total_registries": 2,
    "active_registries": 2,
    "cognitive_complexity": 2
  }
}
EOF
    
    echo "📋 Registry listing generated: $DEMO_DIR/registry_listing.json"
    echo "   Registries discovered: $(grep -o '"total_registries": [0-9]*' "$DEMO_DIR/registry_listing.json" | grep -o '[0-9]*')"
else
    echo "❌ registry-sources.scm not found"
fi

echo ""

# Simulate build profile scanning
echo "🔧 Step 2: Build Profile Scanning"
echo "---------------------------------"

if [ -f "base-devcontainers.scm" ]; then
    echo "✅ Found base-devcontainers.scm"
    echo "⚙️ Simulating profile scanning..."
    
    # Generate profile scan
    cat > "$DEMO_DIR/build_profiles_scan.json" << EOF
{
  "generated": "$(date -u -Iseconds)",
  "profiles": [
    {
      "id": "opencog-dev",
      "name": "OpenCog Development Environment",
      "base_os": "guix-system",
      "features": ["atomspace", "reasoning", "nlp", "python-bindings", "scheme-bindings", "debugging"],
      "guix_variants": ["stable", "latest", "development"],
      "tensor_shape": [10, "feature_count", "package_complexity", "build_time"],
      "status": "ready"
    },
    {
      "id": "atomspace-minimal",
      "name": "AtomSpace Minimal Environment",
      "base_os": "guix-system",
      "features": ["atomspace", "scheme-bindings", "basic-reasoning"],
      "guix_variants": ["stable", "latest"],
      "tensor_shape": [6, "feature_count", "package_complexity", "build_time"],
      "status": "ready"
    },
    {
      "id": "cognitive-agent",
      "name": "Cognitive Agent Runtime",
      "base_os": "guix-system",
      "features": ["runtime", "agent-execution", "minimal-footprint"],
      "guix_variants": ["stable"],
      "tensor_shape": [4, "feature_count", "package_complexity", "build_time"],
      "status": "ready"
    }
  ],
  "summary": {
    "total_profiles": 3,
    "ready_profiles": 3,
    "guix_reproducible": true,
    "cognitive_complexity": 20
  }
}
EOF
    
    echo "📋 Profile scan generated: $DEMO_DIR/build_profiles_scan.json"
    echo "   Profiles available: $(grep -o '"total_profiles": [0-9]*' "$DEMO_DIR/build_profiles_scan.json" | grep -o '[0-9]*')"
else
    echo "❌ base-devcontainers.scm not found"
fi

echo ""

# Generate cognitive health metrics
echo "🧠 Step 3: Meta-Cognitive Health Assessment"
echo "-------------------------------------------"

REGISTRY_COUNT=0
PROFILE_COUNT=0

if [ -f "$DEMO_DIR/registry_listing.json" ]; then
    REGISTRY_COUNT=$(grep -o '"total_registries": [0-9]*' "$DEMO_DIR/registry_listing.json" | grep -o '[0-9]*')
fi

if [ -f "$DEMO_DIR/build_profiles_scan.json" ]; then
    PROFILE_COUNT=$(grep -o '"total_profiles": [0-9]*' "$DEMO_DIR/build_profiles_scan.json" | grep -o '[0-9]*')
fi

cat > "$DEMO_DIR/cognitive_health_metrics.json" << EOF
{
  "timestamp": "$(date -u -Iseconds)",
  "workflow_status": "operational",
  "registry_health": "optimal",
  "profile_health": "ready",
  "cognitive_load": "balanced",
  "hypergraph_expansion": "active",
  "metrics": {
    "registries_active": $REGISTRY_COUNT,
    "profiles_ready": $PROFILE_COUNT,
    "total_cognitive_complexity": $((REGISTRY_COUNT + PROFILE_COUNT * 5)),
    "system_readiness": "100%"
  },
  "recommendations": [
    "Continue registry monitoring",
    "Validate build profiles periodically",
    "Monitor hypergraph expansion patterns"
  ]
}
EOF

echo "📊 Cognitive health assessment: $DEMO_DIR/cognitive_health_metrics.json"
echo "   System readiness: $(grep -o '"system_readiness": "[^"]*"' "$DEMO_DIR/cognitive_health_metrics.json" | cut -d'"' -f4)"

echo ""

# Display summary
echo "📋 Step 4: Ecosystem Summary"
echo "----------------------------"
echo "✅ Registry Discovery: $REGISTRY_COUNT registries active"
echo "✅ Profile Management: $PROFILE_COUNT profiles ready"
echo "✅ Cognitive Health: Optimal"
echo "✅ Hypergraph Expansion: Active"

echo ""
echo "🎯 Demo completed successfully!"
echo "📁 All output files available in: $DEMO_DIR"

# Optional: Display file contents if user wants details
if [ "$1" = "--verbose" ] || [ "$1" = "-v" ]; then
    echo ""
    echo "📄 File Contents:"
    echo "=================="
    
    echo ""
    echo "🔍 Registry Listing:"
    head -20 "$DEMO_DIR/registry_listing.json"
    echo ""
    
    echo "🔧 Build Profiles:"
    head -15 "$DEMO_DIR/build_profiles_scan.json"
    echo ""
    
    echo "🧠 Cognitive Health:"
    cat "$DEMO_DIR/cognitive_health_metrics.json"
fi

echo ""
echo "💡 Usage: $0 [--verbose|-v] to see detailed output"
echo "🌟 This demo showcases the cognitive flowchart implementation"
echo "   for seeding and managing source registries and build profiles."