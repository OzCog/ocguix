#!/bin/bash
# KoboldCpp Setup Script for OpenCog Cognitive Ecosystem
# Implements workflow from issues #69-#76 in the "madness" meta-issue #68
#
# This script:
# 1. Sets up Gitpod workspace environment (#69)
# 2. Integrates with agent-zero deployment (#70)  
# 3. Clones and builds KoboldCpp (#71-#73)
# 4. Downloads and configures GGUF model (#74)
# 5. Starts the inference server (#75)
# 6. Provides connection information (#76)

set -e

echo "🤖 KoboldCpp Setup for OpenCog Cognitive Ecosystem"
echo "=================================================="
echo "🌀 Part of the madness meta-issue #68 implementation"
echo ""

# Configuration
KOBOLDCPP_DIR="$HOME/koboldcpp"
MODEL_DIR="$HOME/models"
MODEL_URL="https://huggingface.co/concedo/KobbleTinyV2-1.1B-GGUF/resolve/main/KobbleTiny-Q4_K.gguf"
MODEL_FILE="KobbleTiny-Q4_K.gguf"
SERVER_PORT=5001

# Function to check if running in Gitpod
is_gitpod() {
    [ "${GITPOD_WORKSPACE_ID:-}" != "" ]
}

# Function to setup Gitpod environment (#69)
setup_gitpod_environment() {
    echo "🐳 Setting up Gitpod environment (issue #69)"
    echo "-------------------------------------------"
    
    if is_gitpod; then
        echo "✅ Running in Gitpod workspace: $GITPOD_WORKSPACE_ID"
        echo "📍 Workspace URL: $GITPOD_WORKSPACE_URL"
        
        # Update package lists
        sudo apt-get update -y
        
        # Install build dependencies
        sudo apt-get install -y build-essential cmake wget curl git python3 python3-pip
        
        echo "✅ Gitpod environment configured"
    else
        echo "ℹ️  Not running in Gitpod, using local environment"
        echo "ℹ️  Note: For full Gitpod integration, run in a Gitpod workspace"
    fi
    echo ""
}

# Function to setup agent-zero integration (#70)
setup_agent_zero_integration() {
    echo "🤖 Setting up agent-zero integration (issue #70)"
    echo "----------------------------------------------"
    
    # Create integration directory
    mkdir -p "$HOME/agent-zero-integration"
    
    # Create a simple integration script
    cat > "$HOME/agent-zero-integration/koboldcpp-agent-zero-bridge.py" << 'EOF'
#!/usr/bin/env python3
"""
KoboldCpp-Agent-Zero Bridge
Provides integration between KoboldCpp local inference and agent-zero framework
"""

import requests
import json
import sys
import time

class KoboldCppBridge:
    def __init__(self, kobold_url="http://localhost:5001"):
        self.kobold_url = kobold_url
        self.api_url = f"{kobold_url}/api/v1"
    
    def check_connection(self):
        """Check if KoboldCpp server is running"""
        try:
            response = requests.get(f"{self.kobold_url}/api/v1/model", timeout=5)
            return response.status_code == 200
        except:
            return False
    
    def generate_text(self, prompt, max_length=100):
        """Generate text using KoboldCpp"""
        if not self.check_connection():
            return "Error: KoboldCpp server not available"
        
        payload = {
            "prompt": prompt,
            "max_length": max_length,
            "temperature": 0.7,
            "top_p": 0.9
        }
        
        try:
            response = requests.post(f"{self.api_url}/generate", json=payload)
            if response.status_code == 200:
                return response.json().get('results', [{}])[0].get('text', '')
            else:
                return f"Error: HTTP {response.status_code}"
        except Exception as e:
            return f"Error: {str(e)}"

if __name__ == "__main__":
    bridge = KoboldCppBridge()
    
    if len(sys.argv) > 1:
        prompt = " ".join(sys.argv[1:])
        result = bridge.generate_text(prompt)
        print(result)
    else:
        print("Usage: python3 koboldcpp-agent-zero-bridge.py <prompt>")
        print(f"Server status: {'Connected' if bridge.check_connection() else 'Disconnected'}")
EOF
    
    chmod +x "$HOME/agent-zero-integration/koboldcpp-agent-zero-bridge.py"
    echo "✅ Agent-zero integration bridge created at $HOME/agent-zero-integration/"
    echo ""
}

# Function to clone KoboldCpp repository (#71)
clone_koboldcpp() {
    echo "📥 Cloning KoboldCpp repository (issue #71)"
    echo "------------------------------------------"
    
    if [ -d "$KOBOLDCPP_DIR" ]; then
        echo "⚠️  KoboldCpp directory already exists, updating..."
        cd "$KOBOLDCPP_DIR"
        git pull
    else
        echo "📥 Cloning https://github.com/LostRuins/koboldcpp.git"
        git clone https://github.com/LostRuins/koboldcpp.git "$KOBOLDCPP_DIR"
    fi
    
    echo "✅ KoboldCpp repository ready at $KOBOLDCPP_DIR"
    echo ""
}

# Function to build KoboldCpp (#72, #73)
build_koboldcpp() {
    echo "🔨 Building KoboldCpp (issues #72, #73)"
    echo "--------------------------------------"
    
    cd "$KOBOLDCPP_DIR"
    echo "📁 Current directory: $(pwd)"
    
    # Build the project
    echo "🔨 Running make..."
    make -j$(nproc) || make  # Try parallel build, fallback to sequential
    
    echo "✅ KoboldCpp built successfully"
    echo ""
}

# Function to download GGUF model (#74)
download_model() {
    echo "📦 Downloading GGUF model (issue #74)"
    echo "------------------------------------"
    
    mkdir -p "$MODEL_DIR"
    cd "$MODEL_DIR"
    
    if [ -f "$MODEL_FILE" ]; then
        echo "✅ Model file already exists: $MODEL_FILE"
        echo "📊 Size: $(du -h $MODEL_FILE | cut -f1)"
    else
        echo "📥 Downloading: $MODEL_URL"
        wget -O "$MODEL_FILE" "$MODEL_URL"
        echo "✅ Model downloaded: $MODEL_FILE"
        echo "📊 Size: $(du -h $MODEL_FILE | cut -f1)"
    fi
    
    echo ""
}

# Function to start KoboldCpp server (#75)
start_server() {
    echo "🚀 Starting KoboldCpp server (issue #75)"
    echo "---------------------------------------"
    
    cd "$KOBOLDCPP_DIR"
    
    # Check if server is already running
    if curl -s "http://localhost:$SERVER_PORT" >/dev/null 2>&1; then
        echo "⚠️  Server already running on port $SERVER_PORT"
        return
    fi
    
    echo "🚀 Starting server with model: $MODEL_DIR/$MODEL_FILE"
    echo "📡 Server will be available at: http://localhost:$SERVER_PORT"
    
    # Start server in background
    nohup python3 koboldcpp.py --model "$MODEL_DIR/$MODEL_FILE" --port "$SERVER_PORT" > /tmp/koboldcpp.log 2>&1 &
    KOBOLD_PID=$!
    
    echo "🔢 Server PID: $KOBOLD_PID"
    echo "📋 Log file: /tmp/koboldcpp.log"
    
    # Wait for server to start
    echo "⏳ Waiting for server to start..."
    for i in {1..30}; do
        if curl -s "http://localhost:$SERVER_PORT" >/dev/null 2>&1; then
            echo "✅ Server started successfully!"
            break
        fi
        echo -n "."
        sleep 2
    done
    
    if ! curl -s "http://localhost:$SERVER_PORT" >/dev/null 2>&1; then
        echo "❌ Server failed to start within 60 seconds"
        echo "📋 Check log: tail /tmp/koboldcpp.log"
        return 1
    fi
    
    echo ""
}

# Function to provide connection information (#76)
show_connection_info() {
    echo "🌐 Connection Information (issue #76)"
    echo "-----------------------------------"
    
    local server_url="http://localhost:$SERVER_PORT"
    
    if is_gitpod; then
        # In Gitpod, construct the public URL
        local gitpod_url=$(echo "$GITPOD_WORKSPACE_URL" | sed "s/https:\/\//https:\/\/$SERVER_PORT-/")
        echo "🌍 Public Gitpod URL: $gitpod_url"
        echo "📱 Mobile access: $gitpod_url"
    else
        echo "🏠 Local URL: $server_url"
        echo "📱 Mobile access: Connect to same network, use server IP:$SERVER_PORT"
    fi
    
    echo "🔗 API endpoint: $server_url/api/v1"
    echo "🤖 Agent-zero bridge: $HOME/agent-zero-integration/koboldcpp-agent-zero-bridge.py"
    
    # Test the connection
    if curl -s "$server_url" >/dev/null 2>&1; then
        echo "✅ Server is running and accessible"
    else
        echo "❌ Server not accessible"
    fi
    
    echo ""
    echo "🧠 Integration with OpenCog Cognitive Ecosystem:"
    echo "   • Use the agent-zero bridge for cognitive task integration"
    echo "   • Model available for reasoning and language processing"
    echo "   • Ready for distributed cognitive grammar networks"
    echo ""
}

# Main execution
main() {
    echo "🎯 Starting complete KoboldCpp setup workflow..."
    echo ""
    
    setup_gitpod_environment      # Issue #69
    setup_agent_zero_integration  # Issue #70
    clone_koboldcpp              # Issue #71
    build_koboldcpp              # Issue #72, #73
    download_model               # Issue #74
    start_server                 # Issue #75
    show_connection_info         # Issue #76
    
    echo "🎉 KoboldCpp setup complete!"
    echo "🌀 Ready for cognitive ecosystem integration"
    echo ""
    echo "📋 Next steps:"
    echo "   1. Test the web interface at the provided URL"
    echo "   2. Use the agent-zero bridge for programmatic access"
    echo "   3. Integrate with OpenCog cognitive agents"
    echo ""
}

# Handle command line arguments
case "${1:-}" in
    --help|-h)
        echo "KoboldCpp Setup Script for OpenCog Cognitive Ecosystem"
        echo ""
        echo "Usage: $0 [options]"
        echo ""
        echo "Options:"
        echo "  --help, -h          Show this help message"
        echo "  --setup-only        Only setup environment, don't start server"
        echo "  --start-only        Only start server (assumes setup is done)"
        echo "  --status            Check server status"
        echo ""
        echo "This script implements the workflow from issues #69-#76 in meta-issue #68"
        exit 0
        ;;
    --setup-only)
        setup_gitpod_environment
        setup_agent_zero_integration
        clone_koboldcpp
        build_koboldcpp
        download_model
        echo "🎯 Setup complete. Run without --setup-only to start server."
        ;;
    --start-only)
        start_server
        show_connection_info
        ;;
    --status)
        if curl -s "http://localhost:$SERVER_PORT" >/dev/null 2>&1; then
            echo "✅ KoboldCpp server is running on port $SERVER_PORT"
        else
            echo "❌ KoboldCpp server is not running"
        fi
        ;;
    *)
        main
        ;;
esac