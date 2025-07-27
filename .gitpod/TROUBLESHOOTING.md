# Troubleshooting Guide - OpenCog Cognitive Ecosystem in Gitpod

This guide helps resolve common issues when deploying the OpenCog Cognitive Ecosystem in Gitpod.

## 🚨 Quick Diagnosis

### Check Deployment Status
```bash
# View current deployment status
cat /tmp/deployment-status.txt

# Check detailed deployment logs
tail -f /tmp/opencog-deploy.log

# Check setup logs
tail -f /tmp/opencog-setup.log
```

### Service Health Check
```bash
# Check if KoboldCpp server is running
curl -s http://localhost:5001 || echo "KoboldCpp not responding"

# Check process status
ps aux | grep -E "(kobold|guix)" | head -5

# Check port availability
netstat -tlnp | grep :5001
```

## 🔧 Common Issues and Solutions

### 1. Gitpod Workspace Startup Issues

#### Problem: Workspace takes too long to start
**Symptoms:**
- Gitpod workspace stuck on "Starting..."
- Build process appears frozen

**Solutions:**
```bash
# Force restart the workspace
# Use Gitpod dashboard to stop and restart

# Check workspace resources
df -h                    # Disk space
free -m                  # Memory usage
cat /proc/loadavg        # CPU load
```

**Prevention:**
- Use prebuilds (enabled in `.gitpod.yml`)
- Avoid large file downloads during startup

#### Problem: Permission denied errors
**Symptoms:**
- `chmod: permission denied`
- Scripts cannot be executed

**Solutions:**
```bash
# Fix script permissions manually
chmod +x .gitpod/deploy.sh
chmod +x .gitpod/setup.sh
chmod +x koboldcpp-setup.sh
chmod +x *.scm

# Re-run setup
./.gitpod/setup.sh
```

### 2. Guix Installation Issues

#### Problem: Guix installation fails
**Symptoms:**
- `guix: command not found`
- Binary download timeouts
- Permission errors during installation

**Solutions:**
```bash
# Check if Guix is available in PATH
which guix || echo "Guix not found"

# Manual Guix setup
cd /tmp
wget https://ftp.gnu.org/gnu/guix/guix-binary-1.4.0.x86_64-linux.tar.xz
tar -xf guix-binary-*.tar.xz
sudo mv var/guix /var/
sudo mv gnu /
export PATH="/var/guix/profiles/per-user/$(whoami)/current-guix/bin:$PATH"

# Test Guix installation
guix --version
```

**Fallback Solution:**
The deployment automatically falls back to system packages if Guix fails:
```bash
# Check if fallback packages were installed
dpkg -l | grep -E "(cmake|python3|guile)"

# Install missing packages manually
sudo apt-get update
sudo apt-get install build-essential cmake python3-dev guile-3.0
```

#### Problem: Guix package installation timeouts
**Symptoms:**
- `guix install` hangs or times out
- Network connection errors
- Substitute download failures

**Solutions:**
```bash
# Use fallback package installation
./.gitpod/deploy.sh --fallback-only

# Or install packages manually
sudo apt-get install -y $(cat << 'EOF'
build-essential
cmake
make
pkg-config
guile-3.0
guile-3.0-dev
python3
python3-pip
libboost-all-dev
cxxtest
git
curl
wget
EOF
)
```

### 3. KoboldCpp Service Issues

#### Problem: KoboldCpp server won't start
**Symptoms:**
- Port 5001 not responding
- `koboldcpp-setup.sh` exits with errors
- Model download failures

**Solutions:**
```bash
# Check KoboldCpp logs
tail -f /tmp/koboldcpp.log

# Manual KoboldCpp setup
./koboldcpp-setup.sh --setup-only
./koboldcpp-setup.sh --start-only

# Check if port is available
lsof -i :5001 || echo "Port 5001 is free"

# Kill existing processes and restart
pkill -f kobold || true
./koboldcpp-setup.sh --start-only
```

#### Problem: Model download fails
**Symptoms:**
- Download timeouts
- Insufficient disk space
- Corrupt model files

**Solutions:**
```bash
# Check disk space
df -h /tmp
df -h $HOME

# Clear temporary files
rm -rf /tmp/models/* 2>/dev/null || true
rm -rf $HOME/models/*.tmp 2>/dev/null || true

# Manual model download
mkdir -p $HOME/models
cd $HOME/models
wget -c https://huggingface.co/concedo/KobbleTinyV2-1.1B-GGUF/resolve/main/KobbleTiny-Q4_K.gguf

# Verify model file
ls -la $HOME/models/
file $HOME/models/*.gguf
```

#### Problem: Web interface not accessible
**Symptoms:**
- Cannot access KoboldCpp web UI
- Gitpod port forwarding issues
- 502 or 503 errors

**Solutions:**
```bash
# Check if server is running locally
curl http://localhost:5001

# Get Gitpod workspace URL
echo "Workspace URL: $GITPOD_WORKSPACE_URL"
echo "KoboldCpp URL: https://5001-$GITPOD_WORKSPACE_ID.ws-$GITPOD_WORKSPACE_CLUSTER_HOST"

# Restart with specific host binding
pkill -f kobold || true
cd $HOME/koboldcpp
python3 koboldcpp.py --model ../models/KobbleTiny-Q4_K.gguf --host 0.0.0.0 --port 5001
```

### 4. Cognitive Agent Issues

#### Problem: Cognitive agents don't respond
**Symptoms:**
- Agent validation fails
- Scheme interpreter errors
- Module not found errors

**Solutions:**
```bash
# Check Guile installation
guile --version

# Test cognitive agent manually
./cognitive-grammar-integration-agent.scm --validate
./cognitive-grammar-integration-agent.scm --test

# Check for missing dependencies
guix install guile-json guile-web || sudo apt-get install guile-3.0-dev

# Debug Scheme code
guile -c "(display \"Guile working\")(newline)"
```

### 5. Network and Connectivity Issues

#### Problem: Download timeouts
**Symptoms:**
- Package downloads fail
- Git clone operations timeout
- API requests fail

**Solutions:**
```bash
# Test network connectivity
ping -c 3 google.com
curl -I https://httpbin.org/status/200

# Use alternative mirrors
export GUIX_SUBSTITUTE_URLS="https://ci.guix.gnu.org https://bordeaux.guix.gnu.org"

# Configure timeouts
export TIMEOUT_NETWORK=60
export RETRIES_MAX=3
```

#### Problem: Port forwarding not working
**Symptoms:**
- Cannot access services from browser
- Gitpod URLs return errors

**Solutions:**
```bash
# Check Gitpod port configuration
gp ports list

# Open port manually
gp ports expose 5001

# Check service binding
netstat -tlnp | grep :5001
lsof -i :5001
```

## 🔍 Advanced Debugging

### Enable Debug Mode
```bash
# Enable verbose logging
export DEBUG=1
export VERBOSE=1

# Re-run deployment with debug info
./.gitpod/deploy.sh

# Check all log files
find /tmp -name "*opencog*" -o -name "*kobold*" -o -name "*cognitive*" | xargs tail -n 20
```

### Performance Diagnostics
```bash
# Check system resources
top -b -n 1 | head -20
iostat 1 3
vmstat 1 3

# Check disk usage
du -sh * | sort -hr | head -10
du -sh /tmp/* | sort -hr | head -10

# Check network usage
iftop -t -s 10 || netstat -i
```

### Service Dependency Check
```bash
# Check all required tools
for tool in git curl wget python3 guile cmake make; do
    command -v $tool && echo "$tool: OK" || echo "$tool: MISSING"
done

# Check Python packages
python3 -c "import requests, flask, numpy; print('Python packages OK')"

# Check Guile modules
guile -c "(use-modules (web client))(display \"Web client OK\")(newline)" 2>/dev/null || echo "Guile web client missing"
```

## 🚑 Emergency Recovery

### Complete Reset
If everything fails, try a complete reset:
```bash
# Stop all services
pkill -f kobold || true
pkill -f guile || true

# Clear temporary files
rm -rf /tmp/opencog-* /tmp/kobold* /tmp/cognitive-*

# Reset workspace permissions
chmod +x *.sh *.scm .gitpod/*.sh

# Full redeploy
./.gitpod/deploy.sh
```

### Minimal Working Environment
Get a basic working environment without all features:
```bash
# Install minimal packages
sudo apt-get update
sudo apt-get install -y build-essential python3 python3-pip git

# Install essential Python packages
pip3 install requests flask

# Test basic functionality
python3 -c "print('Basic environment working')"
./ocpkg --help | head -5
```

## 📞 Getting Help

### Log Collection for Support
```bash
# Collect all relevant logs
mkdir -p /tmp/debug-info
cp /tmp/*opencog* /tmp/debug-info/ 2>/dev/null || true
cp /tmp/*kobold* /tmp/debug-info/ 2>/dev/null || true
cp .gitpod.yml /tmp/debug-info/
ls -la > /tmp/debug-info/file-listing.txt
env | grep -E "(GITPOD|OPENCOG|COGNITIVE)" > /tmp/debug-info/environment.txt

echo "Debug info collected in /tmp/debug-info/"
```

### System Information
```bash
# Gather system info for bug reports
cat << 'EOF' > /tmp/system-info.txt
=== OpenCog Gitpod Environment Debug Info ===

Date: $(date)
Gitpod Workspace: $GITPOD_WORKSPACE_ID
Gitpod URL: $GITPOD_WORKSPACE_URL

=== System Info ===
$(uname -a)
$(lsb_release -a 2>/dev/null || cat /etc/os-release)

=== Resource Usage ===
$(free -m)
$(df -h)

=== Network ===
$(ip addr show | grep inet)

=== Services ===
$(ps aux | grep -E "(kobold|guix|cognitive)" | head -10)

=== Ports ===
$(netstat -tlnp | grep -E ":500[0-9]")

EOF

echo "System info saved to /tmp/system-info.txt"
```

### Community Support
- **GitHub Issues**: https://github.com/OzCog/ocguix/issues
- **Documentation**: See `TECHNICAL-ARCHITECTURE.md`
- **Examples**: Run `./demo-cognitive-flowchart.sh`

---

**Still having issues?** Create a GitHub issue with your log files and system information for personalized help!