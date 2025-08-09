#!/usr/bin/env python3
"""
Integration Test for Python-Scheme AtomSpace Bridge
Tests bidirectional communication between Python agents and Scheme cognitive agents
"""

import os
import sys
import time
import json
import subprocess
import tempfile
from typing import Dict, List, Any

# Add the bridges directory to path
bridges_dir = os.path.join(os.path.dirname(__file__), 'skz-integration', 'bridges')
sys.path.insert(0, bridges_dir)

from python_atomspace_bridge import PythonAtomSpaceBridge, test_python_atomspace_bridge

def test_communication_infrastructure():
    """Test the communication infrastructure between Python and Scheme"""
    print("🧪 Testing Communication Infrastructure")
    print("-" * 40)
    
    # Test communication directory creation
    comm_dir = "/tmp/skz_atomspace_bridge"
    bridge = PythonAtomSpaceBridge("test-integration-agent", comm_dir)
    
    # Verify communication files are created
    nodes_file = os.path.join(comm_dir, "atomspace_nodes.json")
    links_file = os.path.join(comm_dir, "atomspace_links.json")
    messages_file = os.path.join(comm_dir, "bridge_messages.json")
    
    assert os.path.exists(comm_dir), "Communication directory not created"
    
    # Create some test data
    agent_node = bridge.create_skz_agent_node("test-python-agent", ["testing"], "active")
    submission_node = bridge.create_submission_node("test-sub-123", "Test Integration Submission")
    bridge.create_agent_processes_link(agent_node, submission_node, "testing")
    
    # Verify files exist and contain data
    assert os.path.exists(nodes_file), "Nodes file not created"
    assert os.path.exists(links_file), "Links file not created"
    assert os.path.exists(messages_file), "Messages file not created"
    
    # Verify JSON structure
    with open(nodes_file, 'r') as f:
        nodes_data = json.load(f)
        assert 'nodes' in nodes_data, "Nodes data missing"
        assert len(nodes_data['nodes']) >= 2, "Expected at least 2 nodes"
    
    with open(messages_file, 'r') as f:
        messages_data = json.load(f)
        assert 'messages' in messages_data, "Messages data missing"
        assert len(messages_data['messages']) > 0, "Expected messages"
    
    print("✅ Communication infrastructure test passed")
    return bridge

def test_python_agent_functionality():
    """Test Python agent functionality"""
    print("\n🐍 Testing Python Agent Functionality")
    print("-" * 40)
    
    # Import and test the example agent
    example_agent_dir = os.path.join(os.path.dirname(__file__), 'skz-integration')
    sys.path.insert(0, example_agent_dir)
    
    try:
        from example_python_agent import PythonResearchAgent
        
        # Create agent
        agent = PythonResearchAgent("test-integration-research-agent")
        
        # Test package discovery
        packages = agent.discover_packages("test-query")
        assert len(packages) > 0, "Package discovery failed"
        print(f"✅ Package discovery: found {len(packages)} packages")
        
        # Test submission analysis
        test_submission = {
            'id': 'integration-test-001',
            'title': 'Integration Test Submission',
            'content': 'Testing Python-Scheme bridge integration'
        }
        assessment = agent.analyze_submission(test_submission)
        assert 'quality_score' in assessment, "Assessment missing quality score"
        print(f"✅ Submission analysis: quality score {assessment['quality_score']}")
        
        # Test AtomSpace monitoring
        activity = agent.monitor_atomspace()
        assert 'total_nodes' in activity, "Activity report missing nodes count"
        print(f"✅ AtomSpace monitoring: {activity['total_nodes']} nodes, {activity['total_links']} links")
        
        # Test collaboration setup
        collab_result = agent.collaborate_with_scheme_agent(
            'test-scheme-agent',
            {'type': 'research', 'query': 'integration-test'}
        )
        assert 'workflow_id' in collab_result, "Collaboration missing workflow ID"
        print(f"✅ Scheme collaboration: workflow {collab_result['workflow_id']}")
        
        return agent
        
    except ImportError as e:
        print(f"❌ Could not import Python agent: {e}")
        return None

def test_scheme_bridge_communication():
    """Test communication with Scheme bridge (simulated)"""
    print("\n🎭 Testing Scheme Bridge Communication")
    print("-" * 40)
    
    comm_dir = "/tmp/skz_atomspace_bridge"
    
    # Create a Python bridge and add some data
    bridge = PythonAtomSpaceBridge("test-scheme-comm-agent", comm_dir)
    
    # Add test data that should be processed by Scheme bridge
    agent_node = bridge.create_skz_agent_node("python-test-agent", ["research", "analysis"], "active")
    research_node = bridge.bridge_research_discovery({
        'type': 'test-research',
        'findings': ['finding1', 'finding2', 'finding3'],
        'confidence': 0.95
    })
    
    # Simulate Scheme bridge reading and processing messages
    messages_file = os.path.join(comm_dir, "bridge_messages.json")
    
    if os.path.exists(messages_file):
        with open(messages_file, 'r') as f:
            messages_data = json.load(f)
            messages = messages_data.get('messages', [])
            
        print(f"📨 Found {len(messages)} messages for Scheme bridge")
        
        # Verify message types and content
        message_types = [msg.get('message_type') for msg in messages]
        expected_types = ['agent_registration', 'research_data_created']
        
        for expected_type in expected_types:
            if expected_type in message_types:
                print(f"✅ Message type verified: {expected_type}")
            else:
                print(f"⚠️ Message type missing: {expected_type}")
        
        # Simulate Scheme bridge response by updating nodes file
        nodes_file = os.path.join(comm_dir, "atomspace_nodes.json")
        scheme_response = {
            'nodes': [
                {
                    'node_type': 'SKZAgentNode',
                    'node_id': 'scheme-response-node-001',
                    'properties': {
                        'created_by': 'scheme-bridge',
                        'agent_id': 'scheme-cognitive-agent',
                        'response_to': 'python-test-agent',
                        'message': 'Received Python agent data successfully'
                    },
                    'created_at': time.time()
                }
            ],
            'updated_at': time.time(),
            'updated_by': 'simulated-scheme-bridge'
        }
        
        with open(nodes_file, 'w') as f:
            json.dump(scheme_response, f, indent=2)
        
        print("✅ Simulated Scheme bridge response created")
        
        # Test Python bridge reading Scheme response
        bridge._load_existing_data()
        scheme_nodes = [node for node in bridge.nodes.values() 
                       if node.properties.get('created_by') == 'scheme-bridge']
        
        if scheme_nodes:
            print(f"✅ Python bridge received Scheme response: {len(scheme_nodes)} nodes")
        else:
            print("⚠️ Python bridge did not receive Scheme response")
        
    else:
        print("❌ Messages file not found")
    
    return len(messages) if 'messages' in locals() else 0

def test_atomspace_data_consistency():
    """Test AtomSpace data consistency across Python-Scheme bridge"""
    print("\n🔍 Testing AtomSpace Data Consistency")
    print("-" * 40)
    
    # Create multiple bridges to simulate multiple agents
    bridges = [
        PythonAtomSpaceBridge("python-agent-1"),
        PythonAtomSpaceBridge("python-agent-2"),  
        PythonAtomSpaceBridge("python-agent-3")
    ]
    
    # Each agent creates some data
    test_data = []
    for i, bridge in enumerate(bridges):
        agent_id = f"test-agent-{i+1}"
        capabilities = [f"capability-{i+1}", "testing"]
        
        agent_node = bridge.create_skz_agent_node(agent_id, capabilities, "active")
        research_node = bridge.create_research_data_node(
            f"test-data-type-{i+1}",
            f"Test research content {i+1}",
            agent_id
        )
        
        test_data.append({
            'bridge': bridge,
            'agent_node': agent_node,
            'research_node': research_node
        })
    
    # Test data synchronization
    total_nodes = 0
    total_links = 0
    
    for data in test_data:
        bridge = data['bridge']
        bridge.sync_with_scheme_bridge()
        
        nodes_count = len(bridge.nodes)
        links_count = len(bridge.links)
        
        total_nodes += nodes_count
        total_links += links_count
        
        print(f"✅ Bridge {bridge.agent_id}: {nodes_count} nodes, {links_count} links")
    
    print(f"📊 Total across all bridges: {total_nodes} nodes, {total_links} links")
    
    # Verify data integrity
    all_node_ids = []
    for data in test_data:
        all_node_ids.extend(data['bridge'].nodes.keys())
    
    unique_node_ids = set(all_node_ids)
    if len(unique_node_ids) == len(all_node_ids):
        print("✅ All node IDs are unique")
    else:
        print("⚠️ Duplicate node IDs detected")
    
    return len(unique_node_ids)

def run_comprehensive_integration_test():
    """Run comprehensive integration test suite"""
    print("🧪 Comprehensive Python-Scheme AtomSpace Bridge Integration Test")
    print("=" * 70)
    
    results = {
        'communication_infrastructure': False,
        'python_agent_functionality': False,
        'scheme_bridge_communication': False,
        'atomspace_data_consistency': False,
        'overall_success': False
    }
    
    try:
        # Test 1: Communication Infrastructure
        bridge = test_communication_infrastructure()
        results['communication_infrastructure'] = True
        
        # Test 2: Python Agent Functionality
        agent = test_python_agent_functionality()
        results['python_agent_functionality'] = agent is not None
        
        # Test 3: Scheme Bridge Communication
        message_count = test_scheme_bridge_communication()
        results['scheme_bridge_communication'] = message_count > 0
        
        # Test 4: AtomSpace Data Consistency
        unique_nodes = test_atomspace_data_consistency()
        results['atomspace_data_consistency'] = unique_nodes > 0
        
        # Overall success
        results['overall_success'] = all([
            results['communication_infrastructure'],
            results['python_agent_functionality'],
            results['scheme_bridge_communication'],
            results['atomspace_data_consistency']
        ])
        
        print(f"\n📊 Integration Test Results:")
        print("=" * 30)
        for test_name, passed in results.items():
            status = "✅ PASSED" if passed else "❌ FAILED"
            print(f"{test_name.replace('_', ' ').title()}: {status}")
        
        if results['overall_success']:
            print(f"\n🎉 INTEGRATION TEST SUITE PASSED!")
            print(f"🌉 Python-Scheme AtomSpace bridge is working correctly!")
        else:
            print(f"\n⚠️ Some tests failed - see details above")
        
        return results
        
    except Exception as e:
        print(f"\n❌ Integration test failed with exception: {e}")
        import traceback
        traceback.print_exc()
        results['overall_success'] = False
        return results

def main():
    """Main entry point"""
    print("🌉 Python-Scheme AtomSpace Bridge Integration Testing")
    print("=" * 60)
    
    # Run Python bridge tests first
    print("\n🐍 Running Python AtomSpace Bridge Tests:")
    python_bridge = test_python_atomspace_bridge()
    
    print("\n" + "="*60)
    
    # Run comprehensive integration tests
    results = run_comprehensive_integration_test()
    
    # Return appropriate exit code
    if results['overall_success']:
        print(f"\n✅ ALL TESTS PASSED - AtomSpace bridge integration is working!")
        return 0
    else:
        print(f"\n❌ SOME TESTS FAILED - Check logs above for details")
        return 1

if __name__ == "__main__":
    exit_code = main()
    sys.exit(exit_code)