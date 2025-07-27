## Collection of scripts

### 🚀 One-Click Gitpod Deployment (Recommended)

**Get OpenCog running in your browser in 2-3 minutes with zero configuration!**

[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/OzCog/ocguix)

**✨ Complete deployment solution featuring:**
- **Automated Guix installation** with graceful fallbacks
- **KoboldCpp language model server** ready on port 5001
- **Cognitive grammar agents** and distributed processing
- **Comprehensive error handling** and deployment logging
- **Zero-configuration experience** - just click and use!

**🔧 What gets deployed automatically:**
- GNU Guix package manager for reproducible builds
- Complete OpenCog cognitive ecosystem
- KoboldCpp web interface for AI model interaction
- Distributed cognitive grammar processing agents
- Full development environment with VS Code integration

📚 **Deployment Documentation**: [.gitpod/README.md](.gitpod/README.md) | **Troubleshooting**: [.gitpod/TROUBLESHOOTING.md](.gitpod/TROUBLESHOOTING.md)

---

### 🧠 Cognitive Ecosystem (Enhanced with Complete Gitpod Deployment)

This repository includes a complete cognitive ecosystem with **one-click cloud deployment**:
- **KoboldCpp** local language model inference
- **Agent-Zero** agentic framework integration  
- **Distributed cognitive grammar** network processing
- **OpenCog AtomSpace** knowledge representation
- **Guix-based** reproducible environments
- **Comprehensive Gitpod integration** with automated deployment

#### Quick Start (Local)
```bash
# Complete KoboldCpp + Cognitive Grammar setup
./koboldcpp-setup.sh

# Test the cognitive ecosystem
./test-cognitive-flowchart.sh

# Generate distributed cognitive grammar agent
./cognitive-grammar-integration-agent.scm --test
```

See [TECHNICAL-ARCHITECTURE.md](TECHNICAL-ARCHITECTURE.md) for detailed documentation.

#### ocpkg
* This script installs an OpenCog development environment on a fresh
  installation of Ubuntu >= 14.04 . It has options to selectively
  download, build, test, install OpenCog projects.

For a quick start using Ubuntu version >= 14.04, run
```
sudo curl -L http://raw.github.com/opencog/ocpkg/master/ocpkg -o /usr/local/bin/octool &&\
sudo chmod +x /usr/local/bin/octool &&\
octool
```

For details, see the
[instructions on the OpenCog wiki](http://wiki.opencog.org/wikihome/index.php/Building_OpenCog#octool_for_ubuntu).

#### octool-wip
Work-in-progress (abandoned).
The separate octool script is not yet ready. Use the above.

#### ocbootstrap
(This hasn't been tested for a while)
A script to create an OpenCog build environment on ''any'' Linux system.

#### ocfeedbot
An IRC bot of some sort, purpose not clear.

Uses debootstrap. Requires ocpkg.

#### octool_rpi
For installing opencog on a Raspberry Pi Computer running Raspbian.
The readme [here](https://github.com/opencog/opencog_rpi/blob/master/README.md) will be helpful.

May be out of date.

#### Example Usage
* To install all dependencies necessary to build OpenCog:
```
 ./octool -rdpcav -l default
 # Optional: Add -s for installing dependencies for haskell binding.
 # Optional: Add -n for installing dependencies and kernels for jupyter notebooks.
```

* To install all dependencies necessary to build AtomSpace and AS-MOSES:
```
 ./octool -rdcv
```

* To install all dependencies necessary to build Cogutil:
```
 ./octool -rdv
```
