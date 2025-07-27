# OpenCog Cognitive Ecosystem - Gitpod Deployment

This directory contains the complete one-click Gitpod deployment solution for the OpenCog Cognitive Ecosystem. The deployment automates the entire Guix build & deploy process, enabling users to launch a live OpenCog environment directly in their browser with zero configuration.

## 🚀 Quick Start

### One-Click Deployment
Click the "Open in Gitpod" button in the main README.md or use this direct link:

[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/OzCog/ocguix)

### What Happens Automatically
1. **Environment Setup** - Custom Docker image with all dependencies
2. **Guix Installation** - GNU Guix package manager for reproducible builds
3. **Package Installation** - Complete cognitive ecosystem packages
4. **Service Startup** - KoboldCpp and cognitive agents start automatically
5. **Ready to Use** - Full OpenCog environment in ~2-3 minutes

## 📁 Files in This Directory

### Core Deployment Files
- **`deploy.sh`** - Main deployment automation script with comprehensive error handling
- **`setup.sh`** - Environment setup and workspace preparation
- **`manifest.scm`** - Gitpod-optimized Guix package manifest for faster cloud builds
- **`README.md`** - This comprehensive documentation
- **`TROUBLESHOOTING.md`** - Detailed troubleshooting guide

### Configuration Files
- **`../.gitpod.yml`** - Main Gitpod workspace configuration
- **`../.gitpod.Dockerfile`** - Custom Docker image with pre-installed dependencies

## 🔧 Technical Architecture

### Docker Configuration
- **Base Image**: `gitpod/workspace-python-3.10:2025-07-23-06-50-33`
- **Pre-installed**: GNU Guix, build tools, Python ecosystem
- **Optimized**: For cloud environments with network efficiency

### Deployment Process
```
Gitpod Startup
├── Docker Image Load (30s)
├── Environment Setup (30s)
│   ├── Workspace permissions
│   ├── Environment variables
│   └── Gitpod optimizations
├── Guix Installation (60s)
│   ├── Try cognitive bootstrap
│   ├── Fallback to manual install
│   └── Continue without Guix if needed
├── Package Installation (60s)
│   ├── Guix packages (preferred)
│   └── System packages (fallback)
├── Cognitive Environment (30s)
│   ├── KoboldCpp setup
│   ├── Cognitive agent validation
│   └── Script permissions
└── Service Startup (30s)
    ├── KoboldCpp server (port 5001)
    └── Background cognitive services
```

### Fallback Mechanisms
The deployment includes comprehensive fallback support:

1. **Guix Installation Failures**
   - Falls back to system package manager (apt)
   - Continues deployment with essential packages
   - Logs all attempts for debugging

2. **Network Issues**
   - Timeouts on downloads with retry logic
   - Offline-capable operation where possible
   - Clear error messages for network problems

3. **Service Startup Issues**
   - Graceful degradation if services fail
   - Detailed logging for troubleshooting
   - Alternative startup methods

## 🌍 Available Services

### KoboldCpp Language Model Server
- **Port**: 5001
- **Access**: Automatic port forwarding in Gitpod
- **URL Pattern**: `https://5001-{workspace-id}.ws-{cluster}.gitpod.io`
- **Features**: Web UI for language model interaction

### Cognitive Grammar Agents
- **Script**: `cognitive-grammar-integration-agent.scm`
- **Features**: Distributed cognitive processing
- **Testing**: `./cognitive-grammar-integration-agent.scm --test`

### OpenCog Package Management
- **Tool**: `ocpkg`
- **Features**: Complete OpenCog environment management
- **Usage**: `./ocpkg -rdpcav -l default`

## 🎯 Usage Instructions

### Basic Operation
Once deployed, the environment is ready for:

1. **Interactive Development**
   ```bash
   # Test the cognitive pipeline
   ./test-cognitive-flowchart.sh
   
   # Run demonstrations
   ./demo-cognitive-flowchart.sh
   
   # Access KoboldCpp web interface
   # (Opens automatically in Gitpod)
   ```

2. **Advanced Features**
   ```bash
   # Install additional OpenCog components
   ./ocpkg -rdpcav -l default
   
   # Test cognitive agent integration
   ./cognitive-grammar-integration-agent.scm --validate
   
   # Monitor deployment status
   cat /tmp/opencog-deploy.log
   ```

### Development Workflow
1. **Code Editing** - Use Gitpod's VS Code interface
2. **Testing** - Run cognitive tests and demonstrations
3. **Debugging** - Check logs and service status
4. **Sharing** - Share workspace URL with collaborators

## 📊 Monitoring and Logs

### Deployment Logs
- **Main Log**: `/tmp/opencog-deploy.log`
- **Setup Log**: `/tmp/opencog-setup.log`
- **Status File**: `/tmp/deployment-status.txt`
- **KoboldCpp Log**: `/tmp/koboldcpp.log`

### Service Status
```bash
# Check deployment status
cat /tmp/deployment-status.txt

# Check KoboldCpp server
curl -s http://localhost:5001 | head -5

# Check running processes
ps aux | grep -E "(kobold|guix|cognitive)"

# Check port availability
netstat -tlnp | grep :5001
```

### Performance Monitoring
- **Resource Usage**: Available in Gitpod dashboard
- **Service Health**: Automated checks in deployment script
- **Network Status**: Built-in connectivity validation

## 🔍 Customization

### Environment Variables
Set in `.gitpod.yml` or during runtime:
```bash
export KOBOLDCPP_PORT=5001
export COGNITIVE_GRAMMAR_AGENT=distributed-cognitive-grammar-agent.scm
export OPENCOG_BUILD_TYPE=RelWithDebInfo
```

### Package Selection
Modify `.gitpod/manifest.scm` to customize Guix packages:
```scheme
;; Add new packages to the manifest
(specifications->manifest
  '("your-package"
    "another-package"
    ;; ... existing packages
    ))
```

### Service Configuration
Edit deployment scripts for custom behavior:
- `deploy.sh` - Main deployment logic
- `setup.sh` - Environment preparation
- `../koboldcpp-setup.sh` - KoboldCpp configuration

## 🤝 Integration with Existing Infrastructure

### Guix Integration
- **Leverages**: Existing `cognitive-manifest.scm`
- **Enhances**: `guix-cognitive-bootstrap.sh` functionality  
- **Maintains**: Compatibility with local development

### Cognitive Ecosystem
- **KoboldCpp**: Automated setup and integration
- **Agent-Zero**: Framework compatibility
- **OpenCog**: Full environment support
- **Distributed Grammar**: Network-ready agents

### Development Tools
- **VS Code**: Pre-configured extensions
- **Python**: Complete ecosystem support
- **Scheme/Guile**: Cognitive agent development
- **Git**: Full version control integration

## 🎓 Learning Resources

### Documentation
- **Technical Architecture**: `../TECHNICAL-ARCHITECTURE.md`
- **Cognitive Ecosystem**: `../COGNITIVE-ECOSYSTEM.md`
- **Health Status**: `../HEALTH-STATUS.md`
- **Troubleshooting**: `TROUBLESHOOTING.md`

### Example Workflows
- **Cognitive Flowchart**: `../demo-cognitive-flowchart.sh`
- **Vendor Integration**: `../demo-guix-vendor-integration.sh`
- **Network Testing**: `../test-distributed-network-integration.sh`

### Community
- **Repository**: https://github.com/OzCog/ocguix
- **Issues**: Report problems or request features
- **Contributions**: Pull requests welcome

## 📈 Performance Optimization

### Gitpod-Specific Optimizations
- **Prebuilds**: Enabled for faster startup
- **Docker Layers**: Optimized for cache efficiency
- **Package Selection**: Minimal dependencies for speed
- **Resource Usage**: Configured for cloud environments

### Build Performance
- **Parallel Builds**: Enabled where possible
- **Cache Utilization**: Aggressive caching strategies
- **Network Efficiency**: Optimized download patterns
- **Memory Management**: Configured for Gitpod limits

---

**Ready to start?** Click the Gitpod button and experience the future of cognitive computing! 🧠✨