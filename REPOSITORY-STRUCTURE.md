# OpenCog Monorepo Structure

This repository contains the OpenCog cognitive ecosystem as a monorepo, including all 19 required repositories for the build workflow.

## Repository Structure

```
ocguix/
├── repos/                    # OpenCog component repositories
│   ├── cogutil/             # Core utilities (required)
│   ├── atomspace/           # Core AtomSpace (required)
│   ├── atomspace-storage/   # Storage backend (required)
│   ├── atomspace-rocks/     # RocksDB backend (required)
│   ├── atomspace-restful/   # REST API (required)
│   ├── cogserver/           # Network server (required)
│   ├── unify/               # Expression unifier (required)
│   ├── ure/                 # Unified Rule Engine (required)
│   ├── spacetime/           # Spatiotemporal system (required)
│   ├── attention/           # Attention allocation (required)
│   ├── miner/               # Pattern mining (required)
│   ├── pln/                 # Probabilistic Logic Networks (required)
│   ├── moses/               # Machine learning (required)
│   ├── asmoses/             # AtomSpace MOSES (required)
│   ├── lg-atomese/          # Link Grammar integration (required)
│   ├── learn/               # Language learning (required)
│   ├── pattern-index/       # Fast indexing (required)
│   ├── vision/              # Computer vision (required)
│   ├── opencog/             # Main OpenCog framework (required)
│   ├── agent-zero/          # Additional agent system
│   ├── koboldcpp/           # Language model server
│   ├── link-grammar/        # Link Grammar parser
│   └── relex/               # Relation extraction
├── .gitpod/                 # Gitpod configuration
├── .github/workflows/       # CI/CD workflows
├── clone-opencog-repos.sh   # Repository cloning script
└── ...
```

## Repository Categories

### Required Repositories (19)
These are the repositories specified in the `.github/workflows/oc.yml` build workflow:

1. **cogutil** - Core utilities and basic data structures
2. **atomspace** - Hypergraph knowledge representation system
3. **atomspace-storage** - AtomSpace storage backend abstraction
4. **atomspace-rocks** - RocksDB storage backend for AtomSpace
5. **atomspace-restful** - RESTful web API for AtomSpace
6. **cogserver** - Network server for distributed AtomSpace
7. **unify** - Atomese expression unification system
8. **ure** - Unified Rule Engine for inference
9. **spacetime** - 3D+time spatial reasoning system
10. **attention** - Attention allocation subsystem
11. **miner** - Frequent and surprising pattern mining
12. **pln** - Probabilistic Logic Networks
13. **moses** - Meta-Optimizing Semantic Evolutionary Search
14. **asmoses** - AtomSpace version of MOSES
15. **lg-atomese** - Link Grammar integration with Atomese
16. **learn** - Language learning system
17. **pattern-index** - Fast indexing for AtomSpace patterns
18. **vision** - Computer vision integration
19. **opencog** - Main OpenCog framework and integration

### Additional Repositories
- **agent-zero** - Modern AI agent framework
- **koboldcpp** - Language model inference server
- **link-grammar** - Natural language parsing
- **relex** - Relation extraction system

## Build Dependencies

The repositories are designed to be built in dependency order as specified in the workflow:

```
cogutil → atomspace → [storage backends] → [reasoning systems] → [applications]
```

## Deployment

### Gitpod Deployment
The `.gitpod/deploy.sh` script automatically:
1. Checks for all required repositories
2. Clones missing repositories from https://github.com/opencog/
3. Sets up the build environment
4. Starts cognitive services

### Manual Cloning
Use the provided script to clone missing repositories:
```bash
./clone-opencog-repos.sh
```

### Build Workflow
The `.github/workflows/oc.yml` workflow builds all 19 repositories in the correct dependency order.

## Repository Management

### Adding New Repositories
1. Add the repository name to the `REQUIRED_REPOS` array in `clone-opencog-repos.sh`
2. Update the build workflow if needed
3. Add to this documentation

### Updating Repositories
Each repository maintains its own git history as a shallow clone. To update:
```bash
cd repos/repository-name
git pull
```

## Development

### Local Development
1. Clone this monorepo
2. Run `./clone-opencog-repos.sh` to ensure all repositories are available
3. Use the individual repositories for development
4. Build using the workflow or individual CMake builds

### Gitpod Development
Open in Gitpod for automatic environment setup:
- All repositories are automatically cloned
- Build environment is configured
- Cognitive services are started
- Web interface is available on port 5001

## Notes

- Repositories are stored as shallow clones (--depth 1) to reduce size
- Each repository maintains its own git history
- The build system expects all 19 repositories to be present
- Additional repositories are preserved for extended functionality