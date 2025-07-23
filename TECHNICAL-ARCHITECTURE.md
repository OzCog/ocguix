# Technical Architecture Documentation

**Generated for issue #78**: *generate technical architecture documentation with mermaid diagrams*  
**Part of madness meta-issue #68**

## Overview

This document describes the technical architecture of the OpenCog/Guix Cognitive Ecosystem as enhanced by the "madness" initiative (issue #68). The system integrates multiple components to form a distributed network of agentic cognitive grammar processing.

## System Architecture

```mermaid
graph TB
    subgraph "Cognitive Ecosystem Architecture"
        subgraph "User Interfaces"
            WEB[Web Interface<br/>localhost:5001]
            CLI[Command Line Tools<br/>ocpkg, koboldcpp-setup.sh]
            API[REST API<br/>KoboldCpp Server]
        end
        
        subgraph "Cognitive Processing Layer"
            CGA[Cognitive Grammar Agent<br/>distributed-cognitive-grammar-agent.scm]
            RDA[Registry Discovery Agent<br/>registry-discovery-agent.scm]
            PEA[Profile Extraction Agent<br/>profile-extraction-agent.scm]
            ASA[Artifact Synthesis Agent<br/>artifact-synthesis-agent.scm]
            MFA[Meta-Cognitive Feedback Agent<br/>meta-cognitive-feedback-agent.scm]
        end
        
        subgraph "Integration Bridges"
            AZB[Agent-Zero Bridge<br/>koboldcpp-agent-zero-bridge.py]
            OCB[OpenCog Bridge<br/>AtomSpace Integration]
            GB[Guix Bridge<br/>Package Management]
        end
        
        subgraph "Infrastructure Layer"
            KCP[KoboldCpp Server<br/>Language Model Inference]
            AZ[Agent-Zero Framework<br/>Task Orchestration]
            OC[OpenCog AtomSpace<br/>Knowledge Representation]
            GE[Guix Ecosystem<br/>Environment Management]
        end
        
        subgraph "Data Layer"
            GGUF[GGUF Models<br/>KobbleTiny-Q4_K.gguf]
            REG[Registry Sources<br/>GitHub, Guix, Julia]
            MAN[Manifests<br/>cognitive-manifest.scm]
            PROF[Build Profiles<br/>Generated Configurations]
        end
    end
    
    %% User Interface Connections
    WEB --> KCP
    CLI --> CGA
    API --> AZB
    
    %% Cognitive Processing Connections
    CGA --> RDA
    CGA --> PEA
    CGA --> ASA
    CGA --> MFA
    
    %% Bridge Connections
    CGA --> AZB
    CGA --> OCB
    CGA --> GB
    
    %% Infrastructure Connections
    AZB --> KCP
    AZB --> AZ
    OCB --> OC
    GB --> GE
    
    %% Data Connections
    KCP --> GGUF
    RDA --> REG
    ASA --> MAN
    PEA --> PROF
    
    %% Feedback Loops
    MFA --> CGA
    MFA --> RDA
    MFA --> PEA
    MFA --> ASA
    
    style CGA fill:#e1f5fe
    style KCP fill:#f3e5f5
    style AZ fill:#e8f5e8
    style OC fill:#fff3e0
    style GE fill:#fce4ec
```

## Cognitive Grammar Processing Flow

```mermaid
sequenceDiagram
    participant User
    participant CGA as Cognitive Grammar Agent
    participant KCP as KoboldCpp Server
    participant AZ as Agent-Zero Framework
    participant OC as OpenCog AtomSpace
    participant GE as Guix Ecosystem
    
    User->>CGA: Input cognitive query
    CGA->>CGA: Analyze pattern type
    
    alt Query Pattern (Language Generation)
        CGA->>KCP: Send language generation request
        KCP->>KCP: Process with GGUF model
        KCP->>CGA: Return generated text
    
    else Task Pattern (Agentic Execution)
        CGA->>AZ: Delegate task execution
        AZ->>AZ: Plan and execute task
        AZ->>CGA: Return execution result
    
    else Knowledge Pattern (Concept Processing)
        CGA->>OC: Store/query concepts
        OC->>OC: Process in AtomSpace
        OC->>CGA: Return knowledge result
    
    else Dependency Pattern (Package Management)
        CGA->>GE: Resolve dependencies
        GE->>GE: Query package database
        GE->>CGA: Return package information
    end
    
    CGA->>User: Return processed result
    
    Note over CGA,GE: Meta-cognitive feedback loop updates all agents
```

## Component Details

### 1. KoboldCpp Language Model Server (Issues #71-#76)

**Purpose**: Local language model inference  
**Technology**: Python, GGUF models  
**Endpoint**: `http://localhost:5001`

**Key Features**:
- Small footprint GGUF model (KobbleTiny-Q4_K.gguf)
- REST API for text generation
- Web interface for interactive use
- Mobile browser compatibility

**Integration Points**:
- Agent-Zero bridge for programmatic access
- Cognitive Grammar Agent for language processing
- Meta-cognitive feedback for model optimization

### 2. Agent-Zero Framework Integration (Issue #70)

**Purpose**: Agentic task orchestration and execution  
**Technology**: Python bridge integration  
**Repository**: https://github.com/agent0ai/agent-zero

**Bridge Implementation**:
```python
class KoboldCppBridge:
    def generate_text(self, prompt, max_length=100):
        # Sends requests to KoboldCpp API
        # Returns generated text for agent processing
```

### 3. Distributed Cognitive Grammar Network (Issue #77)

**Purpose**: Coordinate between different cognitive systems  
**Technology**: Guile Scheme agents  
**Architecture**: Hypergraph-based routing

**Network Nodes**:
- Language Model Server (KoboldCpp)
- Agentic Orchestrator (Agent-Zero)
- Knowledge Representation (OpenCog)
- Environment Management (Guix)
- Resource Discovery (Registry agents)

**Grammar Patterns**:
- Query patterns → Language model
- Task patterns → Agent-zero
- Knowledge patterns → OpenCog
- Dependency patterns → Guix
- Discovery patterns → Registry agents

### 4. Environment Management (Issues #69, Guix Integration)

**Gitpod Workspace Setup**:
```bash
# Automated via koboldcpp-setup.sh
docker pull gitpod/workspace-python-3.10:2025-07-23-06-50-33
```

**Guix Manifest Dependencies**:
- AI/ML frameworks (numpy, scipy)
- HTTP clients (requests, curl)
- Development tools (gcc, cmake, make)
- Container support (docker)

## Data Flow Architecture

```mermaid
flowchart LR
    subgraph "Input Sources"
        UI[User Input]
        API[API Requests]
        SCHED[Scheduled Tasks]
    end
    
    subgraph "Processing Pipeline"
        direction TB
        PARSE[Pattern Parser]
        ROUTE[Grammar Router]
        EXEC[Execution Engine]
        FB[Feedback Loop]
    end
    
    subgraph "Execution Targets"
        LM[Language Model<br/>KoboldCpp]
        AG[Agent Framework<br/>Agent-Zero]
        KB[Knowledge Base<br/>OpenCog]
        PM[Package Manager<br/>Guix]
        REG[Registry Scanner]
    end
    
    subgraph "Output Formats"
        TEXT[Generated Text]
        TASKS[Executed Tasks]
        KNOW[Knowledge Updates]
        DEPS[Resolved Dependencies]
        META[Registry Metadata]
    end
    
    UI --> PARSE
    API --> PARSE
    SCHED --> PARSE
    
    PARSE --> ROUTE
    ROUTE --> EXEC
    EXEC --> FB
    FB --> ROUTE
    
    EXEC --> LM
    EXEC --> AG
    EXEC --> KB
    EXEC --> PM
    EXEC --> REG
    
    LM --> TEXT
    AG --> TASKS
    KB --> KNOW
    PM --> DEPS
    REG --> META
    
    TEXT --> FB
    TASKS --> FB
    KNOW --> FB
    DEPS --> FB
    META --> FB
```

## Deployment Architecture

```mermaid
deployment
    node "Gitpod Workspace" {
        component "KoboldCpp Server" as KCP
        component "Agent-Zero Bridge" as AZB
        component "Cognitive Agents" as CA
        
        KCP --> AZB : HTTP API
        AZB --> CA : Python Bridge
    }
    
    node "Local Development" {
        component "Guix Environment" as GE
        component "OpenCog AtomSpace" as OC
        component "Build Tools" as BT
        
        GE --> OC : Native Integration
        GE --> BT : Package Management
    }
    
    cloud "External Resources" {
        storage "HuggingFace Models" as HF
        storage "GitHub Repositories" as GH
        storage "Package Registries" as PR
        
        HF --> KCP : Model Download
        GH --> CA : Registry Discovery
        PR --> GE : Package Resolution
    }
    
    database "Local Storage" {
        folder "Models" as MOD
        folder "Profiles" as PROF
        folder "Artifacts" as ART
        
        KCP --> MOD : GGUF Files
        CA --> PROF : Build Profiles
        CA --> ART : Generated Manifests
    }
```

## Security and Resource Considerations

### Resource Requirements
- **CPU**: Moderate (language model inference)
- **Memory**: 2GB+ (GGUF model loading)
- **Storage**: 1GB+ (models and artifacts)
- **Network**: Outbound HTTPS (model downloads, registry access)

### Security Measures
- Local model inference (no external API calls)
- Sandboxed execution environments
- Manifest-driven reproducible builds
- Version-controlled configuration

## API Reference

### KoboldCpp Server API
```
POST /api/v1/generate
{
  "prompt": "string",
  "max_length": 100,
  "temperature": 0.7
}
```

### Cognitive Grammar Agent Interface
```scheme
(process-cognitive-grammar "generate a reasoning framework")
(bridge-to-koboldcpp "explain quantum computing")
(bridge-to-agent-zero "execute code generation task")
```

### Agent-Zero Bridge
```python
bridge = KoboldCppBridge()
result = bridge.generate_text("cognitive architecture design")
```

## Testing and Validation

### Automated Tests
- `koboldcpp-setup.sh --status` - Server health check
- `cognitive-grammar-integration-agent.scm --test` - Integration test
- `test-cognitive-flowchart.sh` - End-to-end pipeline test

### Manual Validation
1. Web interface accessibility at `http://localhost:5001`
2. Agent-zero bridge functionality
3. Cognitive grammar pattern recognition
4. Package dependency resolution

## Future Extensions

### Planned Enhancements
- Multi-model support (larger GGUF models)
- Distributed deployment (multiple nodes)
- Advanced cognitive patterns
- Real-time feedback optimization
- Enhanced security measures

### Integration Opportunities
- Additional AI frameworks (PyTorch, TensorFlow)
- Cloud deployment options
- Mobile application interfaces
- External knowledge bases

---

*This documentation is part of the OpenCog/Guix Cognitive Ecosystem initiative and addresses the requirements specified in issue #68 "madness".*