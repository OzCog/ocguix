#!/usr/bin/env python3
"""
Example Python Agent using AtomSpace Bridge
Demonstrates how Python agents can interact with Scheme cognitive agents via AtomSpace
"""

import sys
import os
import time
import json
from typing import Dict, List, Any

# Add the bridges directory to path
bridges_dir = os.path.join(os.path.dirname(__file__), 'bridges')
sys.path.insert(0, bridges_dir)

from python_atomspace_bridge import PythonAtomSpaceBridge

class PythonResearchAgent:
    """
    Example Python agent that performs research tasks and communicates
    with Scheme cognitive agents via the AtomSpace bridge
    """
    
    def __init__(self, agent_id: str = "python-research-agent"):
        self.agent_id = agent_id
        self.bridge = PythonAtomSpaceBridge(agent_id)
        self.capabilities = [
            'research-discovery',
            'data-analysis', 
            'package-discovery',
            'quality-assessment'
        ]
        
        # Register this agent in the AtomSpace
        self.agent_node = self.bridge.bridge_agent_registration(
            self.agent_id, self.capabilities, 'active'
        )
        
        print(f"🤖 Python Research Agent initialized: {self.agent_id}")
    
    def discover_packages(self, query: str = "opencog") -> List[Dict[str, Any]]:
        """Simulate package discovery research"""
        print(f"🔍 Discovering packages for query: {query}")
        
        # Simulate research results
        research_results = [
            {
                'package_name': 'opencog-atomspace',
                'source': 'github',
                'url': 'https://github.com/opencog/atomspace',
                'confidence': 0.95,
                'relevance': 0.98
            },
            {
                'package_name': 'opencog-cogutil',
                'source': 'github', 
                'url': 'https://github.com/opencog/cogutil',
                'confidence': 0.92,
                'relevance': 0.88
            },
            {
                'package_name': 'guix-opencog',
                'source': 'guix',
                'url': 'gnu/packages/ai.scm',
                'confidence': 0.89,
                'relevance': 0.91
            }
        ]
        
        # Bridge research results to AtomSpace
        for result in research_results:
            research_node = self.bridge.bridge_research_discovery(result, self.agent_id)
            print(f"📊 Research result bridged: {result['package_name']} -> {research_node}")
        
        return research_results
    
    def analyze_submission(self, submission_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze a submission and provide assessment"""
        print(f"📋 Analyzing submission: {submission_data.get('title', 'Unknown')}")
        
        # Bridge submission to AtomSpace
        submission_node = self.bridge.bridge_submission_processing(submission_data, self.agent_id)
        
        # Simulate quality assessment
        assessment = {
            'quality_score': 0.78,
            'safety_score': 0.92,
            'completeness_score': 0.85,
            'recommendation': 'accept-with-modifications',
            'issues': [
                'Missing documentation section',
                'Needs additional test coverage'
            ],
            'strengths': [
                'Well-structured code',
                'Good error handling',
                'Clear API design'
            ]
        }
        
        # Bridge assessment to AtomSpace
        assessment_node = self.bridge.create_assessment_node(
            'quality-assessment',
            assessment['quality_score'],
            assessment['recommendation']
        )
        
        # Create link from submission to assessment
        self.bridge.create_knowledge_flow_link(
            submission_node, assessment_node, 'quality-evaluation'
        )
        
        print(f"✅ Assessment complete: {assessment['recommendation']}")
        return assessment
    
    def collaborate_with_scheme_agent(self, scheme_agent_id: str, task_data: Dict[str, Any]):
        """Demonstrate collaboration with a Scheme cognitive agent"""
        print(f"🤝 Collaborating with Scheme agent: {scheme_agent_id}")
        
        # Create workflow for collaboration
        workflow_data = {
            'workflow_id': f'collaboration-{int(time.time())}',
            'workflow_type': 'python-scheme-collaboration',
            'status': 'active',
            'steps': [
                'python-analysis',
                'data-transfer',
                'scheme-processing',
                'result-synthesis'
            ],
            'participants': [self.agent_id, scheme_agent_id]
        }
        
        workflow_node = self.bridge.bridge_workflow_orchestration(workflow_data)
        
        # Process the task data
        if task_data.get('type') == 'research':
            results = self.discover_packages(task_data.get('query', 'default'))
        elif task_data.get('type') == 'assessment':
            results = self.analyze_submission(task_data.get('submission', {}))
        else:
            results = {'status': 'processed', 'type': task_data.get('type', 'unknown')}
        
        # Bridge collaboration results
        collaboration_data = {
            'workflow_id': workflow_data['workflow_id'],
            'python_agent': self.agent_id,
            'scheme_agent': scheme_agent_id,
            'results': results,
            'status': 'python-processing-complete'
        }
        
        collab_node = self.bridge.bridge_research_discovery(collaboration_data, self.agent_id)
        
        # Link workflow to collaboration results
        self.bridge.create_knowledge_flow_link(workflow_node, collab_node, 'collaboration-result')
        
        print(f"🎯 Collaboration task completed: {workflow_data['workflow_id']}")
        return collaboration_data
    
    def monitor_atomspace(self) -> Dict[str, Any]:
        """Monitor the AtomSpace for updates from Scheme agents"""
        print(f"👁️ Monitoring AtomSpace for Scheme agent updates")
        
        # Sync with Scheme bridge
        self.bridge.sync_with_scheme_bridge()
        
        # Query for recent activity
        all_nodes = self.bridge.query_nodes()
        all_links = self.bridge.query_links()
        
        # Analyze activity patterns
        scheme_nodes = [node for node in all_nodes 
                       if node.properties.get('created_by', '').startswith('skz-')]
        
        activity_report = {
            'total_nodes': len(all_nodes),
            'total_links': len(all_links),
            'scheme_agent_nodes': len(scheme_nodes),
            'python_agent_nodes': len(all_nodes) - len(scheme_nodes),
            'recent_activity': len([node for node in all_nodes 
                                  if time.time() - node.created_at < 300]),  # Last 5 minutes
            'bridge_status': self.bridge.get_bridge_status()
        }
        
        print(f"📊 AtomSpace Activity: {activity_report['total_nodes']} nodes, {activity_report['total_links']} links")
        return activity_report
    
    def run_agent_loop(self, duration: int = 60):
        """Run the agent in a loop to demonstrate ongoing operation"""
        print(f"🔄 Starting agent loop for {duration} seconds")
        
        start_time = time.time()
        iteration = 0
        
        while time.time() - start_time < duration:
            iteration += 1
            print(f"\n--- Agent Loop Iteration {iteration} ---")
            
            # Perform different tasks in rotation
            if iteration % 3 == 1:
                # Research task
                self.discover_packages(f"opencog-{iteration}")
                
            elif iteration % 3 == 2:
                # Assessment task
                test_submission = {
                    'id': f'test-submission-{iteration}',
                    'title': f'Test Package Submission #{iteration}',
                    'content': 'Example package for cognitive computing',
                    'status': 'pending'
                }
                self.analyze_submission(test_submission)
                
            else:
                # Collaboration task
                self.collaborate_with_scheme_agent(
                    'skz-editorial-orchestration',
                    {
                        'type': 'research',
                        'query': f'cognitive-computing-{iteration}'
                    }
                )
            
            # Monitor AtomSpace
            activity = self.monitor_atomspace()
            
            # Wait before next iteration
            time.sleep(10)
        
        print(f"\n✅ Agent loop completed after {iteration} iterations")


def main():
    """Main entry point for the Python agent"""
    print("🤖 Python Agent with AtomSpace Bridge")
    print("=" * 50)
    
    # Create and initialize agent
    agent = PythonResearchAgent()
    
    # Demonstrate different capabilities
    print(f"\n🔬 Demonstrating Package Discovery:")
    packages = agent.discover_packages("cognitive-computing")
    
    print(f"\n📋 Demonstrating Submission Analysis:")
    test_submission = {
        'id': 'demo-submission-001',
        'title': 'Demo Cognitive Computing Package',
        'content': 'A comprehensive package for cognitive computing applications',
        'author': 'demo-developer',
        'status': 'pending-review'
    }
    assessment = agent.analyze_submission(test_submission)
    
    print(f"\n🤝 Demonstrating Scheme Agent Collaboration:")
    collab_result = agent.collaborate_with_scheme_agent(
        'skz-research-discovery',
        {
            'type': 'research',
            'query': 'atomspace-integration'
        }
    )
    
    print(f"\n👁️ Demonstrating AtomSpace Monitoring:")
    activity = agent.monitor_atomspace()
    
    print(f"\n📊 Final Bridge Status:")
    status = agent.bridge.get_bridge_status()
    print(json.dumps(status, indent=2))
    
    print(f"\n🎉 Python Agent demonstration completed successfully!")
    print(f"🌉 AtomSpace bridge is active with {status['nodes_count']} nodes and {status['links_count']} links")
    
    # Optionally run agent loop
    import sys
    if len(sys.argv) > 1 and sys.argv[1] == '--loop':
        duration = int(sys.argv[2]) if len(sys.argv) > 2 else 120
        agent.run_agent_loop(duration)


if __name__ == "__main__":
    main()