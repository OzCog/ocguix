# GNU Guix Shepherd DevContainer for OpenCog

This directory contains a complete **GNU Guix Shepherd devcontainer setup** for turning the OpenCog repository into Guix packages, providing a reproducible development environment as requested in issue #206.

## Overview

This FSF-approved setup combines:
- **Debian bookworm** as the container base
- **GNU Guix** package manager for reproducible environments
- **GNU Shepherd** service management for build/test automation  
- **OpenCog** package definitions in pure Scheme
- **VSCode DevContainer** integration for seamless development

## Quick Start

1. **Open in DevContainer**: Use VSCode or compatible IDE with DevContainer support
2. **Wait for Setup**: The container will automatically install Guix, Shepherd, and dependencies
3. **Start Shepherd**: Run `./shepherd-manager.sh start`
4. **Build OpenCog**: Run `./shepherd-manager.sh build`

## File Structure

```
.devcontainer/
├── Dockerfile              # Debian + Guix + Shepherd setup
├── devcontainer.json       # DevContainer configuration
└── setup-environment.sh    # Environment initialization

.config/shepherd/
└── init.scm                # Shepherd service definitions

opencog.scm                 # OpenCog package definition
shepherd-manager.sh         # Service management script
test-shepherd-deployment.sh # Validation test suite
```

## Shepherd Services

The setup provides several Shepherd services for OpenCog development:

- **opencog-build**: Build OpenCog packages using Guix
- **opencog-test**: Run OpenCog tests with `--check`
- **cogutil-build**: Build cogutil dependency
- **dev-environment**: Set up pure Guix development shell

## Usage Commands

### Service Management
```bash
# Start shepherd daemon and services
./shepherd-manager.sh start

# Show service status
./shepherd-manager.sh status

# Stop shepherd daemon
./shepherd-manager.sh stop

# Restart shepherd
./shepherd-manager.sh restart
```

### Build and Test
```bash
# Build OpenCog via shepherd
./shepherd-manager.sh build

# Run tests via shepherd  
./shepherd-manager.sh test

# Check build logs
tail -f /tmp/opencog-build.log

# Check test logs
tail -f /tmp/opencog-test.log
```

### Direct Guix Commands
```bash
# Build OpenCog package directly
guix build -f opencog.scm

# Enter development shell
guix shell -f opencog.scm

# Build with tests
guix build -f opencog.scm --check
```

### Shepherd Service Control
```bash
# Start specific service
herd start opencog-build

# Stop specific service  
herd stop opencog-build

# Show all services
herd status

# Show service details
herd status opencog-build
```

## Package Definitions

### opencog.scm
Defines two packages:
- **cogutil**: OpenCog utilities library
- **opencog**: Main OpenCog framework

Both packages:
- Use git-checkout for latest source
- Build with CMake
- Include tests (`#:tests? #t`)
- Support Guile 3.0 and Python bindings

### Integration with Existing guix.scm
The new `opencog.scm` complements the existing `guix.scm` which handles the vendored cogutil setup. Both can be used together:

```bash
# Build existing ocguix package (with vendored cogutil)
guix build -f guix.scm

# Build new OpenCog packages (from git)
guix build -f opencog.scm
```

## Validation

Run the test suite to validate the setup:

```bash
./test-shepherd-deployment.sh
```

This checks:
- File structure and permissions
- Dockerfile content and configuration
- DevContainer JSON validity
- Package definitions syntax
- Shepherd service configuration
- Integration with existing repository

## Why This Setup?

- **Portable**: Anyone can clone and instantly get a working Guix+Shepherd environment
- **Reproducible**: Container and Guix manage all dependencies declaratively  
- **FSF-Sanctioned**: No proprietary layers, just Debian/Guix, AGPL, and GNU tools
- **Scheme Madness**: Shepherd lets you script build/test servers as real Scheme services
- **Mad Scientist Lab**: Extensible to produce multiple package variants, system definitions, or full GuixOS images

## Extending the Setup

### Adding New Services
Edit `.config/shepherd/init.scm` to add new services:

```scheme
(define my-service
  (service
   '(my-service)
   #:start (make-forkexec-constructor '("my-command"))
   #:stop (make-kill-destructor)))

(register-services my-service)
```

### Adding Dependencies
Edit `opencog.scm` to add new inputs:

```scheme
(inputs
 (list boost
       guile-3.0
       python
       my-new-dependency))
```

### Custom Build Phases
Add custom build phases in the package definition:

```scheme
#:phases
(modify-phases %standard-phases
  (add-before 'configure 'my-phase
    (lambda* (#:key inputs #:allow-other-keys)
      ;; Custom build logic
      #t)))
```

## Troubleshooting

### Guix Installation Issues
If Guix fails to install in the container:
```bash
# Check Guix daemon
sudo systemctl status guix-daemon

# Restart if needed
sudo systemctl restart guix-daemon
```

### Shepherd Won't Start
```bash
# Check for existing shepherd processes
pkill shepherd

# Start with debug output
shepherd --config=.config/shepherd/init.scm --verbose
```

### Build Failures
```bash
# Check build logs
cat /tmp/opencog-build.log

# Try building directly
guix build -f opencog.scm --verbosity=3
```

### Package Not Found
If packages fail to build, ensure the channel is up to date:
```bash
guix pull
guix package --list-available | grep opencog
```

## References

- [GNU Guix Manual](https://guix.gnu.org/manual/)
- [GNU Shepherd Manual](https://www.gnu.org/software/shepherd/manual/)
- [OpenCog Project](https://opencog.org/)
- [DevContainer Specification](https://containers.dev/)
- Issue #206: Deploy Shepherd