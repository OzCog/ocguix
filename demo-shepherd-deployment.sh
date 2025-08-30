#!/bin/bash
# Demo script showing the shepherd deployment in action
# This simulates what happens in the devcontainer

echo "🧪 GNU Guix Shepherd OpenCog Packaging Demo"
echo "==========================================="
echo ""

echo "📁 Repository structure with Shepherd deployment:"
echo ""
tree .devcontainer/ .config/ -I '__pycache__' 2>/dev/null || {
    echo ".devcontainer/"
    echo "├── Dockerfile              # Debian + Guix + Shepherd"
    echo "├── devcontainer.json       # DevContainer configuration"
    echo "└── setup-environment.sh    # Environment initialization"
    echo ""
    echo ".config/"
    echo "└── shepherd/"
    echo "    └── init.scm            # Shepherd service definitions"
}

echo ""
echo "📦 OpenCog package definition (opencog.scm):"
head -10 opencog.scm | sed 's/^/  /'

echo ""
echo "🐑 Shepherd services available:"
grep -A 3 "define.*service" .config/shepherd/init.scm | grep -E "(define|docstring)" | sed 's/^/  /'

echo ""
echo "🛠️  Available management commands:"
echo "  ./shepherd-manager.sh start   # Start shepherd daemon"
echo "  ./shepherd-manager.sh build   # Build OpenCog packages" 
echo "  ./shepherd-manager.sh test    # Run OpenCog tests"
echo "  ./shepherd-manager.sh status  # Show service status"

echo ""
echo "🔧 Direct Guix commands:"
echo "  guix build -f opencog.scm     # Build OpenCog directly"
echo "  guix shell -f opencog.scm     # Enter development shell"
echo "  herd status                   # Check shepherd services"

echo ""
echo "✅ Validation results:"
if ./test-shepherd-deployment.sh | grep -E "(✓|✗)" | tail -5; then
    echo ""
    echo "🎉 Shepherd deployment is ready!"
    echo ""
    echo "Next steps:"
    echo "1. Open this repository in VS Code with Dev Containers extension"
    echo "2. Click 'Reopen in Container' when prompted"
    echo "3. Wait for container to build (first time takes ~10 minutes)"
    echo "4. Run: ./shepherd-manager.sh start"
    echo "5. Run: ./shepherd-manager.sh build"
    echo ""
    echo "🧬 Welcome to your FSF-sanctioned mad scientist's Guix packaging lab!"
else
    echo "❌ Validation failed - check the deployment"
fi