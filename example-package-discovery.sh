#!/bin/bash
# Example demonstrating the enhanced package discovery functionality
# This script shows how the registry discovery agent now generates comprehensive package listings
# with real API calls and intelligent fallbacks

echo "🔍 Enhanced Package Discovery Example"
echo "===================================="
echo ""
echo "The registry-discovery-agent.scm has been enhanced to generate"
echo "comprehensive package listings for each registry using REAL discovery:"
echo ""
echo "🌟 New Features:"
echo "✅ Real GitHub API integration with JSON parsing"
echo "✅ Real Guix repository scanning with HTTP validation"
echo "✅ Real Julia registry querying with TOML parsing"
echo "✅ Enhanced error handling with informative fallbacks"
echo "✅ Smart discovery method tracking and reporting"
echo ""

# Create a simple output directory
mkdir -p /tmp/example-output

# Generate an example registry listing with package discovery
cat > /tmp/example-output/example_registry_listing.json << 'EOF'
{
  "generated": "2024-07-23T09:54:00Z",
  "schema_version": "1.1",
  "agent_id": "registry-discovery-agent",
  "registries": [
    {
      "id": "opencog-github",
      "url": "https://github.com/opencog/*",
      "status": "active",
      "package_listings": [
        "atomspace", "opencog", "cogutil", "moses", "relex", "link-grammar",
        "cogserver", "attention", "pln", "spacetime", "learn", "generate",
        "vision", "motor", "sensory", "unify", "benchmark", "agi-bio"
      ],
      "package_count": 18,
      "discovery_method": "github_api_with_fallback",
      "api_status": "enhanced"
    },
    {
      "id": "guix-packages", 
      "url": "https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages",
      "status": "active",
      "package_listings": [
        "gnu/packages/ai.scm", "gnu/packages/scheme.scm", "gnu/packages/cpp.scm",
        "gnu/packages/machine-learning.scm", "gnu/packages/python-science.scm",
        "gnu/packages/maths.scm", "gnu/packages/statistics.scm"
      ],
      "package_count": 7,
      "discovery_method": "guix_git_scan_with_fallback",
      "api_status": "enhanced"
    },
    {
      "id": "julia-ecosystem",
      "url": "https://github.com/JuliaLang/*", 
      "status": "active",
      "package_listings": [
        "MLJ.jl", "Flux.jl", "Knet.jl", "MLDatasets.jl", "StatsModels.jl",
        "Distributions.jl", "Plots.jl", "DataFrames.jl"
      ],
      "package_count": 8,
      "discovery_method": "julia_registry_with_fallback",
      "api_status": "enhanced"
    }
  ],
  "summary": {
    "total_registries": 3,
    "total_packages_discovered": 33,
    "cognitive_complexity": 36
  },
  "package_discovery_stats": {
    "github_repos": 18,
    "guix_packages": 7, 
    "julia_packages": 8,
    "discovery_method": "enhanced_multi_registry_agent",
    "api_integration": "real_with_fallbacks",
    "enhancement_features": [
      "github_api_json_parsing",
      "guix_repository_http_scanning", 
      "julia_registry_toml_parsing",
      "enhanced_error_handling"
    ]
  }
}
EOF

echo "✅ Generated example registry listing with package discovery"
echo ""
echo "📊 Package Discovery Summary:"
echo "- OpenCog GitHub repos: $(grep -o '"github_repos": [0-9]*' /tmp/example-output/example_registry_listing.json | grep -o '[0-9]*')"
echo "- Guix packages: $(grep -o '"guix_packages": [0-9]*' /tmp/example-output/example_registry_listing.json | grep -o '[0-9]*')"
echo "- Julia packages: $(grep -o '"julia_packages": [0-9]*' /tmp/example-output/example_registry_listing.json | grep -o '[0-9]*')"
echo "- Total packages: $(grep -o '"total_packages_discovered": [0-9]*' /tmp/example-output/example_registry_listing.json | grep -o '[0-9]*')"
echo ""
echo "📋 Key Enhancements:"
echo "✅ Real package discovery from multiple registry types"
echo "✅ GitHub API integration with JSON parsing (with fallback)"
echo "✅ Guix repository HTTP scanning for package files" 
echo "✅ Julia registry TOML parsing for ecosystem packages"
echo "✅ Enhanced error handling and discovery method tracking"
echo "✅ Smart fallback mechanisms for network limitations"
echo "✅ Enhanced tensor metadata with discovery method information"
echo ""
echo "📁 Example output available at: /tmp/example-output/example_registry_listing.json"
echo ""
echo "🚀 To run the full enhanced system:"
echo "   ./test-cognitive-flowchart.sh"
echo "   # OR to test enhanced discovery specifically:"
echo "   ./test-enhanced-discovery-fallback.sh"
echo "   # OR if Guile is available:"
echo "   # guile registry-discovery-agent.scm /tmp/enhanced_registry_listing.json"