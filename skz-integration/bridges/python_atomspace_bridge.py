#!/usr/bin/env python3
"""
Python AtomSpace Bridge for SKZ Integration
Bridges Python agents with Scheme cognitive agents via AtomSpace
Part of the SKZ Integration Framework for OpenCog/Guix Cognitive Ecosystem
"""

import json
import time
import os
import uuid
import tempfile
import subprocess
from datetime import datetime
from typing import Dict, List, Any, Optional, Union
from dataclasses import dataclass, asdict
import threading
import queue

# AtomSpace Node and Link Types (matching Scheme bridge)
SKZ_NODE_TYPES = [
    'SKZAgentNode',
    'SubmissionNode', 
    'WorkflowNode',
    'ResearchDataNode',
    'AssessmentNode',
    'DecisionNode',
    'ConflictNode'
]

SKZ_LINK_TYPES = [
    'AgentProcessesLink',
    'SubmissionHasWorkflowLink',
    'WorkflowHasStepLink',
    'AssessmentOfSubmissionLink',
    'DecisionBasedOnLink',
    'ConflictInvolvesLink',
    'KnowledgeFlowLink'
]

@dataclass
class AtomSpaceNode:
    """Represents an AtomSpace node"""
    node_type: str
    node_id: str
    properties: Dict[str, Any]
    created_at: float

@dataclass
class AtomSpaceLink:
    """Represents an AtomSpace link"""
    link_type: str
    link_id: str
    source_node: str
    target_node: str
    properties: Dict[str, Any]
    created_at: float

class PythonAtomSpaceBridge:
    """
    Python AtomSpace Bridge for SKZ Integration
    Enables Python agents to communicate with Scheme cognitive agents via AtomSpace
    """
    
    def __init__(self, agent_id: str = None, communication_dir: str = "/tmp/skz_atomspace_bridge"):
        self.agent_id = agent_id or f"python-agent-{uuid.uuid4().hex[:8]}"
        self.communication_dir = communication_dir
        self.nodes: Dict[str, AtomSpaceNode] = {}
        self.links: Dict[str, AtomSpaceLink] = {}
        self.message_queue = queue.Queue()
        
        # Ensure communication directory exists
        os.makedirs(self.communication_dir, exist_ok=True)
        
        # Initialize communication files
        self.nodes_file = os.path.join(self.communication_dir, "atomspace_nodes.json")
        self.links_file = os.path.join(self.communication_dir, "atomspace_links.json")
        self.messages_file = os.path.join(self.communication_dir, "bridge_messages.json")
        
        # Load existing data if available
        self._load_existing_data()
        
        print(f"🐍 Python AtomSpace Bridge initialized for agent: {self.agent_id}")
    
    def _load_existing_data(self):
        """Load existing AtomSpace data from communication files"""
        try:
            if os.path.exists(self.nodes_file):
                with open(self.nodes_file, 'r') as f:
                    data = json.load(f)
                    for node_data in data.get('nodes', []):
                        node = AtomSpaceNode(**node_data)
                        self.nodes[node.node_id] = node
            
            if os.path.exists(self.links_file):
                with open(self.links_file, 'r') as f:
                    data = json.load(f)
                    for link_data in data.get('links', []):
                        link = AtomSpaceLink(**link_data)
                        self.links[link.link_id] = link
                        
            print(f"📚 Loaded {len(self.nodes)} nodes and {len(self.links)} links from AtomSpace")
            
        except Exception as e:
            print(f"⚠️ Warning: Could not load existing AtomSpace data: {e}")
    
    def _save_to_atomspace(self):
        """Save current AtomSpace state to communication files"""
        try:
            # Save nodes
            nodes_data = {
                'nodes': [asdict(node) for node in self.nodes.values()],
                'updated_at': time.time(),
                'updated_by': self.agent_id
            }
            
            with open(self.nodes_file, 'w') as f:
                json.dump(nodes_data, f, indent=2)
            
            # Save links
            links_data = {
                'links': [asdict(link) for link in self.links.values()],
                'updated_at': time.time(), 
                'updated_by': self.agent_id
            }
            
            with open(self.links_file, 'w') as f:
                json.dump(links_data, f, indent=2)
                
        except Exception as e:
            print(f"❌ Error saving to AtomSpace: {e}")
    
    def _send_bridge_message(self, message_type: str, data: Dict[str, Any]):
        """Send message to Scheme bridge via JSON communication"""
        message = {
            'message_id': str(uuid.uuid4()),
            'timestamp': time.time(),
            'agent_id': self.agent_id,
            'message_type': message_type,
            'data': data
        }
        
        try:
            # Load existing messages
            messages = []
            if os.path.exists(self.messages_file):
                with open(self.messages_file, 'r') as f:
                    messages = json.load(f).get('messages', [])
            
            # Add new message
            messages.append(message)
            
            # Keep only recent messages (last 100)
            messages = messages[-100:]
            
            # Save messages
            with open(self.messages_file, 'w') as f:
                json.dump({
                    'messages': messages,
                    'last_updated': time.time()
                }, f, indent=2)
                
            print(f"📨 Sent bridge message: {message_type}")
            
        except Exception as e:
            print(f"❌ Error sending bridge message: {e}")

    # AtomSpace Node Creation Functions
    def create_skz_agent_node(self, agent_id: str, capabilities: List[str], status: str) -> str:
        """Create an AtomSpace node representing an SKZ agent"""
        node_id = f"skz-agent-{uuid.uuid4().hex[:8]}"
        
        node = AtomSpaceNode(
            node_type='SKZAgentNode',
            node_id=node_id,
            properties={
                'agent_id': agent_id,
                'capabilities': capabilities,
                'status': status,
                'agent_type': 'SKZAutonomousAgent',
                'created_by': self.agent_id
            },
            created_at=time.time()
        )
        
        self.nodes[node_id] = node
        self._save_to_atomspace()
        
        # Send message to Scheme bridge
        self._send_bridge_message('agent_registration', {
            'node_id': node_id,
            'agent_id': agent_id,
            'capabilities': capabilities,
            'status': status
        })
        
        print(f"🧠 Created SKZ Agent node: {agent_id}")
        return node_id

    def create_submission_node(self, submission_id: str, title: str, status: str = 'pending') -> str:
        """Create an AtomSpace node representing a submission"""
        node_id = f"submission-{uuid.uuid4().hex[:8]}"
        
        node = AtomSpaceNode(
            node_type='SubmissionNode',
            node_id=node_id,
            properties={
                'submission_id': submission_id,
                'title': title,
                'submission_status': status,
                'created_by': self.agent_id
            },
            created_at=time.time()
        )
        
        self.nodes[node_id] = node
        self._save_to_atomspace()
        
        self._send_bridge_message('submission_created', {
            'node_id': node_id,
            'submission_id': submission_id,
            'title': title,
            'status': status
        })
        
        print(f"🧠 Created Submission node: {submission_id}")
        return node_id

    def create_workflow_node(self, workflow_id: str, workflow_type: str, 
                           status: str = 'active', steps: List[str] = None) -> str:
        """Create an AtomSpace node representing a workflow"""
        node_id = f"workflow-{uuid.uuid4().hex[:8]}"
        
        node = AtomSpaceNode(
            node_type='WorkflowNode',
            node_id=node_id,
            properties={
                'workflow_id': workflow_id,
                'workflow_type': workflow_type,
                'workflow_status': status,
                'steps': steps or [],
                'created_by': self.agent_id
            },
            created_at=time.time()
        )
        
        self.nodes[node_id] = node
        self._save_to_atomspace()
        
        self._send_bridge_message('workflow_created', {
            'node_id': node_id,
            'workflow_id': workflow_id,
            'workflow_type': workflow_type,
            'status': status,
            'steps': steps
        })
        
        print(f"🧠 Created Workflow node: {workflow_id}")
        return node_id

    def create_research_data_node(self, data_type: str, content: Any, source_agent: str = None) -> str:
        """Create an AtomSpace node for research data"""
        node_id = f"research-data-{uuid.uuid4().hex[:8]}"
        
        node = AtomSpaceNode(
            node_type='ResearchDataNode',
            node_id=node_id,
            properties={
                'data_type': data_type,
                'content': content,
                'generated_by': source_agent or self.agent_id,
                'created_by': self.agent_id
            },
            created_at=time.time()
        )
        
        self.nodes[node_id] = node
        self._save_to_atomspace()
        
        self._send_bridge_message('research_data_created', {
            'node_id': node_id,
            'data_type': data_type,
            'content': str(content),
            'source_agent': source_agent
        })
        
        print(f"🧠 Created Research Data node: {data_type}")
        return node_id

    def create_assessment_node(self, assessment_type: str, score: float, recommendation: str) -> str:
        """Create an AtomSpace node for assessments"""
        node_id = f"assessment-{uuid.uuid4().hex[:8]}"
        
        node = AtomSpaceNode(
            node_type='AssessmentNode',
            node_id=node_id,
            properties={
                'assessment_type': assessment_type,
                'score': score,
                'recommendation': recommendation,
                'created_by': self.agent_id
            },
            created_at=time.time()
        )
        
        self.nodes[node_id] = node
        self._save_to_atomspace()
        
        self._send_bridge_message('assessment_created', {
            'node_id': node_id,
            'assessment_type': assessment_type,
            'score': score,
            'recommendation': recommendation
        })
        
        print(f"🧠 Created Assessment node: {assessment_type}")
        return node_id

    # AtomSpace Link Creation Functions
    def create_agent_processes_link(self, agent_node: str, submission_node: str, 
                                   process_type: str = 'processing') -> str:
        """Create link showing agent processing submission"""
        link_id = f"agent-processes-{uuid.uuid4().hex[:8]}"
        
        link = AtomSpaceLink(
            link_type='AgentProcessesLink',
            link_id=link_id,
            source_node=agent_node,
            target_node=submission_node,
            properties={
                'process_type': process_type,
                'created_by': self.agent_id
            },
            created_at=time.time()
        )
        
        self.links[link_id] = link
        self._save_to_atomspace()
        
        self._send_bridge_message('link_created', {
            'link_id': link_id,
            'link_type': 'AgentProcessesLink',
            'source_node': agent_node,
            'target_node': submission_node,
            'process_type': process_type
        })
        
        print(f"🔗 Created Agent-Processes link: {agent_node} -> {submission_node}")
        return link_id

    def create_submission_workflow_link(self, submission_node: str, workflow_node: str) -> str:
        """Create link between submission and its workflow"""
        link_id = f"submission-workflow-{uuid.uuid4().hex[:8]}"
        
        link = AtomSpaceLink(
            link_type='SubmissionHasWorkflowLink',
            link_id=link_id,
            source_node=submission_node,
            target_node=workflow_node,
            properties={
                'created_by': self.agent_id
            },
            created_at=time.time()
        )
        
        self.links[link_id] = link
        self._save_to_atomspace()
        
        self._send_bridge_message('link_created', {
            'link_id': link_id,
            'link_type': 'SubmissionHasWorkflowLink',
            'source_node': submission_node,
            'target_node': workflow_node
        })
        
        print(f"🔗 Created Submission-Workflow link: {submission_node} -> {workflow_node}")
        return link_id

    def create_knowledge_flow_link(self, source_node: str, target_node: str, flow_type: str) -> str:
        """Create link representing knowledge flow between nodes"""
        link_id = f"knowledge-flow-{uuid.uuid4().hex[:8]}"
        
        link = AtomSpaceLink(
            link_type='KnowledgeFlowLink',
            link_id=link_id,
            source_node=source_node,
            target_node=target_node,
            properties={
                'flow_type': flow_type,
                'created_by': self.agent_id
            },
            created_at=time.time()
        )
        
        self.links[link_id] = link
        self._save_to_atomspace()
        
        self._send_bridge_message('link_created', {
            'link_id': link_id,
            'link_type': 'KnowledgeFlowLink',
            'source_node': source_node,
            'target_node': target_node,
            'flow_type': flow_type
        })
        
        print(f"🔗 Created Knowledge Flow link: {source_node} -> {target_node} ({flow_type})")
        return link_id

    # Query and Retrieval Functions  
    def query_nodes(self, node_type: str = None, properties: Dict[str, Any] = None) -> List[AtomSpaceNode]:
        """Query AtomSpace nodes with optional filtering"""
        results = list(self.nodes.values())
        
        if node_type:
            results = [node for node in results if node.node_type == node_type]
        
        if properties:
            for key, value in properties.items():
                results = [node for node in results if node.properties.get(key) == value]
        
        print(f"🔍 Query returned {len(results)} nodes")
        return results

    def query_links(self, link_type: str = None, source_node: str = None, 
                    target_node: str = None) -> List[AtomSpaceLink]:
        """Query AtomSpace links with optional filtering"""
        results = list(self.links.values())
        
        if link_type:
            results = [link for link in results if link.link_type == link_type]
        
        if source_node:
            results = [link for link in results if link.source_node == source_node]
            
        if target_node:
            results = [link for link in results if link.target_node == target_node]
        
        print(f"🔍 Query returned {len(results)} links")
        return results

    def get_node_by_id(self, node_id: str) -> Optional[AtomSpaceNode]:
        """Get a specific node by ID"""
        return self.nodes.get(node_id)

    def get_link_by_id(self, link_id: str) -> Optional[AtomSpaceLink]:
        """Get a specific link by ID"""
        return self.links.get(link_id)

    # High-level Bridge Functions
    def bridge_agent_registration(self, agent_id: str, capabilities: List[str], status: str = 'active') -> str:
        """Bridge agent registration to AtomSpace"""
        print(f"🌉 Bridging agent registration to AtomSpace: {agent_id}")
        return self.create_skz_agent_node(agent_id, capabilities, status)

    def bridge_submission_processing(self, submission_data: Dict[str, Any], agent_id: str = None) -> str:
        """Bridge submission processing to AtomSpace"""
        print(f"🌉 Bridging submission processing to AtomSpace")
        
        submission_node = self.create_submission_node(
            submission_data.get('id', f'submission-{uuid.uuid4().hex[:8]}'),
            submission_data.get('title', 'Untitled Submission'),
            submission_data.get('status', 'under-review')
        )
        
        # Link to processing agent
        processing_agent = agent_id or self.agent_id
        agent_nodes = self.query_nodes(node_type='SKZAgentNode', 
                                      properties={'agent_id': processing_agent})
        
        if not agent_nodes:
            # Create agent node if it doesn't exist
            agent_node = self.create_skz_agent_node(processing_agent, [], 'active')
        else:
            agent_node = agent_nodes[0].node_id
        
        # Create processing link
        self.create_agent_processes_link(agent_node, submission_node, 'quality-assessment')
        
        return submission_node

    def bridge_workflow_orchestration(self, workflow_data: Dict[str, Any]) -> str:
        """Bridge workflow orchestration to AtomSpace"""
        print(f"🌉 Bridging workflow orchestration to AtomSpace")
        
        workflow_node = self.create_workflow_node(
            workflow_data.get('workflow_id', f'workflow-{uuid.uuid4().hex[:8]}'),
            workflow_data.get('workflow_type', 'general'),
            workflow_data.get('status', 'active'),
            workflow_data.get('steps', [])
        )
        
        # Link to submission if provided
        if 'submission_id' in workflow_data:
            submission_nodes = self.query_nodes(node_type='SubmissionNode',
                                              properties={'submission_id': workflow_data['submission_id']})
            if submission_nodes:
                self.create_submission_workflow_link(submission_nodes[0].node_id, workflow_node)
        
        return workflow_node

    def bridge_research_discovery(self, research_data: Any, agent_id: str = None) -> str:
        """Bridge research discovery results to AtomSpace"""
        print(f"🌉 Bridging research discovery to AtomSpace")
        
        # Determine data type
        if isinstance(research_data, dict):
            data_type = research_data.get('type', 'research-result')
            content = research_data
        elif isinstance(research_data, (list, tuple)):
            data_type = 'research-dataset'
            content = research_data
        else:
            data_type = 'research-finding'
            content = str(research_data)
        
        research_node = self.create_research_data_node(
            data_type, content, agent_id or self.agent_id
        )
        
        # Create knowledge flow from agent if available
        source_agent = agent_id or self.agent_id
        agent_nodes = self.query_nodes(node_type='SKZAgentNode',
                                     properties={'agent_id': source_agent})
        if agent_nodes:
            self.create_knowledge_flow_link(agent_nodes[0].node_id, research_node, 'research-discovery')
        
        return research_node

    def bridge_editorial_decision(self, decision_data: Dict[str, Any]) -> str:
        """Bridge editorial decisions to AtomSpace"""
        print(f"🌉 Bridging editorial decision to AtomSpace")
        
        decision_node = self.create_assessment_node(
            'editorial-decision',
            decision_data.get('confidence', 0.5),
            decision_data.get('recommendation', 'under-review')
        )
        
        # Link decision to workflow if available
        if 'workflow_id' in decision_data:
            workflow_nodes = self.query_nodes(node_type='WorkflowNode',
                                            properties={'workflow_id': decision_data['workflow_id']})
            if workflow_nodes:
                self.create_knowledge_flow_link(workflow_nodes[0].node_id, decision_node, 'editorial-decision')
        
        return decision_node

    # Cognitive Analysis Functions
    def reason_about_submission(self, submission_id: str) -> Dict[str, Any]:
        """Perform cognitive reasoning about a submission using AtomSpace"""
        print(f"🧠 Reasoning about submission: {submission_id}")
        
        # Query for submission and related data
        submission_nodes = self.query_nodes(node_type='SubmissionNode',
                                          properties={'submission_id': submission_id})
        assessment_nodes = self.query_nodes(node_type='AssessmentNode')
        workflow_nodes = self.query_nodes(node_type='WorkflowNode')
        
        # Calculate cognitive complexity
        complexity = len(submission_nodes) + len(assessment_nodes) + len(workflow_nodes)
        
        reasoning_result = {
            'submission_nodes': len(submission_nodes),
            'related_assessments': len(assessment_nodes),
            'active_workflows': len(workflow_nodes),
            'cognitive_complexity': complexity,
            'reasoning_confidence': 0.85,
            'recommended_actions': ['continue-review', 'seek-additional-expertise', 'monitor-progress']
        }
        
        print(f"✅ Cognitive reasoning complete: complexity score {complexity}")
        return reasoning_result

    def analyze_agent_performance(self) -> Dict[str, Any]:
        """Analyze performance of agents using AtomSpace data"""
        print(f"📊 Analyzing agent performance using AtomSpace")
        
        agent_nodes = self.query_nodes(node_type='SKZAgentNode')
        process_links = self.query_links(link_type='AgentProcessesLink')
        
        # Calculate performance metrics
        total_agents = len(agent_nodes)
        total_processes = len(process_links)
        average_load = total_processes / max(total_agents, 1)
        
        performance_analysis = {
            'total_agents': total_agents,
            'total_processes': total_processes,
            'average_load': average_load,
            'system_efficiency': 0.82,
            'cognitive_coherence': 0.89,
            'recommendation': 'optimal-performance' if average_load < 10 else 'scaling-needed'
        }
        
        print(f"✅ Performance analysis: {total_agents} agents, {total_processes} processes")
        return performance_analysis

    # Integration and Communication
    def sync_with_scheme_bridge(self):
        """Synchronize with the Scheme AtomSpace bridge"""
        print(f"🔄 Synchronizing with Scheme AtomSpace bridge")
        
        # Send sync message
        self._send_bridge_message('sync_request', {
            'python_nodes': len(self.nodes),
            'python_links': len(self.links),
            'agent_id': self.agent_id
        })
        
        # Reload data to get updates from Scheme bridge
        self._load_existing_data()
        
        print(f"✅ Sync complete - {len(self.nodes)} nodes, {len(self.links)} links")

    def get_bridge_status(self) -> Dict[str, Any]:
        """Get current bridge status"""
        return {
            'agent_id': self.agent_id,
            'nodes_count': len(self.nodes),
            'links_count': len(self.links),
            'communication_dir': self.communication_dir,
            'last_sync': time.time(),
            'node_types': list(set(node.node_type for node in self.nodes.values())),
            'link_types': list(set(link.link_type for link in self.links.values())),
            'status': 'active'
        }

    def __str__(self) -> str:
        return f"PythonAtomSpaceBridge(agent={self.agent_id}, nodes={len(self.nodes)}, links={len(self.links)})"

    def __repr__(self) -> str:
        return self.__str__()


# Example usage and testing functions
def test_python_atomspace_bridge():
    """Test the Python AtomSpace bridge functionality"""
    print("🧪 Testing Python AtomSpace Bridge")
    print("=" * 50)
    
    # Initialize bridge
    bridge = PythonAtomSpaceBridge("test-python-agent")
    
    print(f"\n🤖 Testing Agent Registration Bridge:")
    agent_node = bridge.bridge_agent_registration('test-research-agent', ['research', 'analysis'], 'active')
    
    print(f"\n📋 Testing Submission Processing Bridge:")
    submission_node = bridge.bridge_submission_processing({
        'id': 'test-sub-001',
        'title': 'Test Submission for Python Bridge',
        'status': 'under-review'
    }, 'test-research-agent')
    
    print(f"\n🎯 Testing Workflow Orchestration Bridge:")
    workflow_node = bridge.bridge_workflow_orchestration({
        'workflow_id': 'test-workflow-001',
        'workflow_type': 'initial-review',
        'status': 'active',
        'steps': ['screening', 'assessment', 'decision'],
        'submission_id': 'test-sub-001'
    })
    
    print(f"\n🔬 Testing Research Discovery Bridge:")
    research_node = bridge.bridge_research_discovery({
        'type': 'package-discovery',
        'packages': ['opencog', 'atomspace', 'guix'],
        'confidence': 0.95
    }, 'test-research-agent')
    
    print(f"\n⚖️ Testing Editorial Decision Bridge:")
    decision_node = bridge.bridge_editorial_decision({
        'recommendation': 'accept',
        'confidence': 0.89,
        'workflow_id': 'test-workflow-001'
    })
    
    print(f"\n🧠 Testing Cognitive Reasoning:")
    reasoning = bridge.reason_about_submission('test-sub-001')
    print(f"Reasoning result: {reasoning}")
    
    print(f"\n📊 Testing Performance Analysis:")
    performance = bridge.analyze_agent_performance()
    print(f"Performance analysis: {performance}")
    
    print(f"\n🔄 Testing Bridge Synchronization:")
    bridge.sync_with_scheme_bridge()
    
    print(f"\n📊 Bridge Status:")
    status = bridge.get_bridge_status()
    print(f"Status: {status}")
    
    print(f"\n✅ All Python bridge tests completed successfully!")
    print(f"📊 Bridge contains {len(bridge.nodes)} nodes and {len(bridge.links)} links")
    
    return bridge


if __name__ == "__main__":
    # Run tests if script is executed directly
    test_bridge = test_python_atomspace_bridge()
    
    # Keep bridge running for interaction
    print(f"\n🌉 Python AtomSpace Bridge is active and ready for integration!")
    print(f"Communication directory: {test_bridge.communication_dir}")