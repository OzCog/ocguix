# Python-Scheme AtomSpace Bridge Documentation

## Overview

The Python-Scheme AtomSpace Bridge enables seamless communication between Python agents and Scheme cognitive agents through a shared AtomSpace. This bridge is part of the SKZ Integration Framework for the OpenCog/Guix Cognitive Ecosystem.

## Architecture

### Components

1. **Python AtomSpace Bridge** (`python_atomspace_bridge.py`)
   - Main Python module for AtomSpace integration
   - Provides Python API for creating nodes and links
   - Handles communication with Scheme bridge

2. **Scheme AtomSpace Bridge** (`skz-atomspace-bridge.scm`) 
   - Existing Scheme bridge enhanced with Python communication
   - Processes messages from Python agents
   - Maintains AtomSpace state

3. **Communication Protocol**
   - JSON-based file communication in `/tmp/skz_atomspace_bridge/`
   - Shared node and link storage
   - Message passing for real-time updates

### Node Types

The bridge supports the following AtomSpace node types:

- `SKZAgentNode` - Represents autonomous agents
- `SubmissionNode` - Represents submissions under review
- `WorkflowNode` - Represents processing workflows  
- `ResearchDataNode` - Represents research findings
- `AssessmentNode` - Represents quality assessments
- `DecisionNode` - Represents editorial decisions
- `ConflictNode` - Represents conflicts requiring resolution

### Link Types

The bridge supports the following AtomSpace link types:

- `AgentProcessesLink` - Agent processing submission
- `SubmissionHasWorkflowLink` - Submission associated with workflow
- `WorkflowHasStepLink` - Workflow containing steps
- `AssessmentOfSubmissionLink` - Assessment of submission quality
- `DecisionBasedOnLink` - Decision based on assessment
- `ConflictInvolvesLink` - Conflict involving agents/submissions
- `KnowledgeFlowLink` - Knowledge transfer between entities

## Usage

### Basic Python Usage

```python
from python_atomspace_bridge import PythonAtomSpaceBridge

# Initialize bridge
bridge = PythonAtomSpaceBridge("my-python-agent")

# Register agent
agent_node = bridge.bridge_agent_registration(
    "research-agent",
    ["research", "analysis"], 
    "active"
)

# Process submission
submission_node = bridge.bridge_submission_processing({
    'id': 'sub-001',
    'title': 'New Package Submission',
    'status': 'pending'
})

# Create research data
research_node = bridge.bridge_research_discovery({
    'type': 'package-analysis',
    'findings': ['good structure', 'needs tests'],
    'confidence': 0.85
})

# Sync with Scheme bridge
bridge.sync_with_scheme_bridge()
```

### Example Python Agent

```python
from python_atomspace_bridge import PythonAtomSpaceBridge

class MyPythonAgent:
    def __init__(self, agent_id):
        self.bridge = PythonAtomSpaceBridge(agent_id)
        self.agent_node = self.bridge.bridge_agent_registration(
            agent_id, ["custom-capability"], "active"
        )
    
    def process_submission(self, submission_data):
        # Process the submission
        submission_node = self.bridge.bridge_submission_processing(
            submission_data, self.agent_id
        )
        
        # Create assessment
        assessment = self.analyze_submission(submission_data)
        assessment_node = self.bridge.create_assessment_node(
            'quality-assessment',
            assessment['score'],
            assessment['recommendation'] 
        )
        
        # Link submission to assessment
        self.bridge.create_knowledge_flow_link(
            submission_node, assessment_node, 'quality-evaluation'
        )
        
        return assessment
```

### Scheme Bridge Integration

The Scheme bridge automatically processes messages from Python agents:

```scheme
;; Sync with Python agents
(sync-with-python-bridge)

;; Process Python messages
(read-python-bridge-messages)

;; Export data for Python agents
(sync-atomspace-data)
```

## Communication Protocol

### File Structure

```
/tmp/skz_atomspace_bridge/
├── atomspace_nodes.json      # Shared node storage
├── atomspace_links.json      # Shared link storage  
└── bridge_messages.json      # Message passing
```

### Message Format

Python agents send messages to Scheme bridge in JSON format:

```json
{
  "message_id": "unique-id",
  "timestamp": 1234567890.123,
  "agent_id": "python-agent-name", 
  "message_type": "agent_registration",
  "data": {
    "agent_id": "research-agent",
    "capabilities": ["research", "analysis"],
    "status": "active"
  }
}
```

### Message Types

- `agent_registration` - Register new agent
- `submission_created` - New submission created
- `workflow_created` - New workflow created
- `research_data_created` - Research data generated
- `assessment_created` - Assessment completed
- `sync_request` - Request data synchronization

## Testing

### Run Python Bridge Tests

```bash
python3 skz-integration/bridges/python_atomspace_bridge.py
```

### Run Example Python Agent

```bash
python3 skz-integration/example_python_agent.py
```

### Run Comprehensive Integration Tests

```bash
python3 test-python-scheme-atomspace-bridge.py
```

### Test Scheme Bridge

```bash
# Note: Requires Guile installation
guile skz-integration/bridges/skz-atomspace-bridge.scm --test
```

## API Reference

### PythonAtomSpaceBridge Class

#### Constructor
```python
PythonAtomSpaceBridge(agent_id=None, communication_dir="/tmp/skz_atomspace_bridge")
```

#### Core Methods

**Node Creation:**
- `create_skz_agent_node(agent_id, capabilities, status)` 
- `create_submission_node(submission_id, title, status)`
- `create_workflow_node(workflow_id, workflow_type, status, steps)`
- `create_research_data_node(data_type, content, source_agent)`
- `create_assessment_node(assessment_type, score, recommendation)`

**Link Creation:**
- `create_agent_processes_link(agent_node, submission_node, process_type)`
- `create_submission_workflow_link(submission_node, workflow_node)`
- `create_knowledge_flow_link(source_node, target_node, flow_type)`

**Query Methods:**
- `query_nodes(node_type=None, properties=None)`
- `query_links(link_type=None, source_node=None, target_node=None)`
- `get_node_by_id(node_id)`
- `get_link_by_id(link_id)`

**Bridge Methods:**
- `bridge_agent_registration(agent_id, capabilities, status)`
- `bridge_submission_processing(submission_data, agent_id)`
- `bridge_workflow_orchestration(workflow_data)`
- `bridge_research_discovery(research_data, agent_id)`
- `bridge_editorial_decision(decision_data)`

**Analysis Methods:**
- `reason_about_submission(submission_id)`
- `analyze_agent_performance()`

**Communication:**
- `sync_with_scheme_bridge()`
- `get_bridge_status()`

## Integration with SKZ Framework

The Python-Scheme AtomSpace Bridge integrates with the existing SKZ autonomous agents framework:

### SKZ Agents
- `skz-research-discovery-agent.scm`
- `skz-submission-assistant-agent.scm` 
- `skz-editorial-orchestration-agent.scm`

### Integration Points
- Shared AtomSpace for knowledge representation
- Common node and link types
- Message-based communication protocol
- Cognitive reasoning capabilities

## Performance Considerations

- Communication via JSON files (suitable for development/testing)
- In-memory AtomSpace simulation in Python
- Periodic synchronization with Scheme bridge
- Message queue with size limits (100 messages)

## Future Enhancements

1. **Direct OpenCog Integration**: Replace JSON communication with direct OpenCog AtomSpace API
2. **Real-time Communication**: WebSocket or TCP-based communication
3. **Distributed AtomSpace**: Support for distributed AtomSpace across multiple nodes
4. **Performance Optimization**: Caching and indexing for large AtomSpaces
5. **Security**: Authentication and authorization for agent communication

## Troubleshooting

### Common Issues

1. **Import Errors**: Ensure Python path includes bridges directory
2. **Permission Errors**: Check write access to `/tmp/skz_atomspace_bridge/`
3. **JSON Errors**: Verify JSON file integrity in communication directory
4. **Sync Issues**: Check file timestamps and agent IDs

### Debug Mode

Enable debug logging:
```python
import logging
logging.basicConfig(level=logging.DEBUG)
```

### Communication Directory

Clean up communication directory:
```bash
rm -rf /tmp/skz_atomspace_bridge/
```

## Examples

See the following files for complete examples:
- `skz-integration/bridges/python_atomspace_bridge.py` - Core bridge implementation
- `skz-integration/example_python_agent.py` - Example Python agent
- `test-python-scheme-atomspace-bridge.py` - Integration tests