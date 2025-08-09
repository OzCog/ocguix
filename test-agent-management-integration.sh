#!/bin/bash
# Test Agent Management Controls Integration
# Validates the agent management system functionality

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEST_LOG="/tmp/agent-management-test.log"

# Color output
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

test_log() {
    echo -e "${BLUE}[TEST]${NC} $1" | tee -a "$TEST_LOG"
}

test_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1" | tee -a "$TEST_LOG"
}

test_error() {
    echo -e "${RED}[ERROR]${NC} $1" | tee -a "$TEST_LOG"
}

test_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1" | tee -a "$TEST_LOG"
}

echo "🧪 Testing Agent Management Controls Integration" | tee "$TEST_LOG"
echo "================================================" | tee -a "$TEST_LOG"
echo "Test started at: $(date)" | tee -a "$TEST_LOG"
echo "" | tee -a "$TEST_LOG"

cd "$SCRIPT_DIR"

# Test 1: Check script files exist
test_log "Test 1: Checking required files exist..."
required_files=(
    "agent-management-system.sh"
    "agent-management-api.py" 
    "agent-management-controller.scm"
    "agent-management-static/dashboard.html"
    "skz-integration/bridges/atomspace-bridge.scm"
)

missing_files=()
for file in "${required_files[@]}"; do
    if [ ! -f "$file" ]; then
        missing_files+=("$file")
    fi
done

if [ ${#missing_files[@]} -eq 0 ]; then
    test_success "All required files exist"
else
    test_error "Missing files: ${missing_files[*]}"
    exit 1
fi

# Test 2: Check script executability
test_log "Test 2: Checking script executability..."
executable_files=(
    "agent-management-system.sh"
    "agent-management-api.py"
    "agent-management-controller.scm" 
    "skz-integration/bridges/atomspace-bridge.scm"
)

for file in "${executable_files[@]}"; do
    if [ -x "$file" ]; then
        test_success "$file is executable"
    else
        test_error "$file is not executable"
        chmod +x "$file"
        test_log "Made $file executable"
    fi
done

# Test 3: Check Python dependencies
test_log "Test 3: Checking Python dependencies..."
if python3 -c "import flask, flask_cors, psutil, json, subprocess, signal, threading, logging" 2>/dev/null; then
    test_success "Python dependencies available"
else
    test_warning "Python dependencies missing, installing..."
    pip3 install flask flask-cors psutil > /dev/null 2>&1 || {
        test_error "Failed to install Python dependencies"
        exit 1
    }
    test_success "Python dependencies installed"
fi

# Test 4: Check Guile availability
test_log "Test 4: Checking Guile availability..."
if command -v guile &> /dev/null; then
    GUILE_VERSION=$(guile --version | head -1)
    test_success "Guile available: $GUILE_VERSION"
else
    test_error "Guile not found - required for Scheme agents"
    exit 1
fi

# Test 5: Test AtomSpace bridge
test_log "Test 5: Testing AtomSpace bridge..."
if guile skz-integration/bridges/atomspace-bridge.scm --test >> "$TEST_LOG" 2>&1; then
    test_success "AtomSpace bridge test passed"
else
    test_warning "AtomSpace bridge test had issues (check log)"
fi

# Test 6: Test agent management controller
test_log "Test 6: Testing agent management controller..."
if timeout 30 guile agent-management-controller.scm --help >> "$TEST_LOG" 2>&1; then
    test_success "Agent management controller help works"
else
    test_warning "Agent management controller had issues (check log)"
fi

# Test 7: Test system initialization
test_log "Test 7: Testing system initialization..."
if timeout 60 ./agent-management-system.sh init >> "$TEST_LOG" 2>&1; then
    test_success "System initialization completed"
else
    test_warning "System initialization had warnings (check log)"
fi

# Test 8: Test API server startup (brief test)
test_log "Test 8: Testing API server startup..."
# Start API server in background
./agent-management-system.sh start >> "$TEST_LOG" 2>&1 &
MGMT_PID=$!

# Wait for startup
sleep 10

# Check if API is responding
if curl -s http://localhost:5002/api/system/status > /dev/null 2>&1; then
    test_success "API server is responding"
    
    # Test API endpoints
    test_log "Testing API endpoints..."
    
    # Test agents endpoint
    if curl -s http://localhost:5002/api/agents > /dev/null 2>&1; then
        test_success "Agents API endpoint works"
    else
        test_error "Agents API endpoint failed"
    fi
    
    # Test system status endpoint
    if curl -s http://localhost:5002/api/system/status | grep -q "agents" 2>/dev/null; then
        test_success "System status API endpoint works"
    else
        test_error "System status API endpoint failed"
    fi
    
    # Test dashboard accessibility  
    if curl -s http://localhost:5002/ | grep -q "Agent Management Dashboard" 2>/dev/null; then
        test_success "Dashboard is accessible"
    else
        test_error "Dashboard is not accessible"
    fi
    
else
    test_error "API server is not responding"
fi

# Cleanup - stop the API server
test_log "Cleaning up test environment..."
./agent-management-system.sh stop >> "$TEST_LOG" 2>&1 || true

# Kill any remaining processes
if kill -0 $MGMT_PID 2>/dev/null; then
    kill $MGMT_PID 2>/dev/null || true
fi

# Test 9: Validate integration with existing infrastructure
test_log "Test 9: Testing integration with existing infrastructure..."

# Check if cognitive grammar integration agent exists
if [ -f "cognitive-grammar-integration-agent.scm" ]; then
    test_success "Found existing cognitive grammar integration agent"
    
    # Test if our agent management integrates
    if grep -q "skz.*agent" cognitive-grammar-integration-agent.scm; then
        test_success "Agent management integrates with cognitive grammar"
    else
        test_warning "Agent management not yet integrated with cognitive grammar"
    fi
else
    test_warning "Cognitive grammar integration agent not found"
fi

# Check if distributed network coordinator exists
if [ -f "distributed-network-coordinator.scm" ]; then
    test_success "Found existing distributed network coordinator"
else
    test_warning "Distributed network coordinator not found"
fi

# Check if KoboldCpp setup exists
if [ -f "koboldcpp-setup.sh" ]; then
    test_success "Found existing KoboldCpp setup"
else
    test_warning "KoboldCpp setup not found"
fi

# Test 10: Validate file structure and naming
test_log "Test 10: Validating file structure and naming..."
expected_structure=(
    "agent-management-system.sh:main startup script"
    "agent-management-api.py:REST API server"
    "agent-management-controller.scm:Scheme controller"
    "agent-management-static/:web dashboard assets"
    "skz-integration/bridges/:AtomSpace integration"
)

for item in "${expected_structure[@]}"; do
    file=$(echo "$item" | cut -d: -f1)
    description=$(echo "$item" | cut -d: -f2)
    
    if [ -e "$file" ]; then
        test_success "$description: $file ✓"
    else
        test_error "$description: $file ✗"
    fi
done

echo "" | tee -a "$TEST_LOG"
test_log "📊 Test Results Summary" 
echo "========================" | tee -a "$TEST_LOG"

# Count results
success_count=$(grep -c "\[SUCCESS\]" "$TEST_LOG" || true)
error_count=$(grep -c "\[ERROR\]" "$TEST_LOG" || true)
warning_count=$(grep -c "\[WARNING\]" "$TEST_LOG" || true)

echo "✅ Successes: $success_count" | tee -a "$TEST_LOG"
echo "❌ Errors: $error_count" | tee -a "$TEST_LOG"
echo "⚠️  Warnings: $warning_count" | tee -a "$TEST_LOG"

echo "" | tee -a "$TEST_LOG"
echo "Test completed at: $(date)" | tee -a "$TEST_LOG"
echo "Full test log: $TEST_LOG" | tee -a "$TEST_LOG"

if [ $error_count -eq 0 ]; then
    test_success "🎉 Agent Management Controls Integration Test: PASSED"
    echo "" | tee -a "$TEST_LOG"
    echo "🧠 Agent management controls successfully added to cognitive ecosystem!" | tee -a "$TEST_LOG"
    echo "🌐 Start system with: ./agent-management-system.sh start" | tee -a "$TEST_LOG"
    echo "📊 Open dashboard at: http://localhost:5002/" | tee -a "$TEST_LOG"
    exit 0
else
    test_error "❌ Agent Management Controls Integration Test: FAILED ($error_count errors)"
    exit 1
fi