#!/usr/bin/env python3
"""
Agent Management API Server
Provides REST API for managing agents in the OpenCog cognitive ecosystem
Part of Phase 3: Frontend Integration for SKZ autonomous agents framework
"""

import os
import json
import time
import subprocess
import signal
import psutil
from datetime import datetime
from flask import Flask, jsonify, request, send_from_directory
from flask_cors import CORS
import threading
import logging

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

app = Flask(__name__)
CORS(app)

# Configuration
AGENTS_DIR = os.path.dirname(os.path.abspath(__file__))
SKZ_AGENTS_DIR = os.path.join(AGENTS_DIR, 'skz-integration', 'autonomous-agents-framework')
BRIDGES_DIR = os.path.join(AGENTS_DIR, 'skz-integration', 'bridges')
PID_DIR = "/tmp/skz-agents-pids"
LOG_DIR = "/tmp/skz-agents-logs"
STATIC_DIR = os.path.join(AGENTS_DIR, 'agent-management-static')

# Ensure directories exist
os.makedirs(PID_DIR, exist_ok=True)
os.makedirs(LOG_DIR, exist_ok=True)
os.makedirs(STATIC_DIR, exist_ok=True)

# Agent definitions
AGENTS = {
    'skz-research-discovery': {
        'name': 'Research Discovery Agent',
        'description': 'INCI database mining, patent analysis, trend identification',
        'script': os.path.join(SKZ_AGENTS_DIR, 'skz-research-discovery-agent.scm'),
        'capabilities': ['inci-database-mining', 'patent-analysis', 'trend-identification'],
        'type': 'cognitive-agent',
        'dependencies': [],
        'auto_restart': True
    },
    'skz-submission-assistant': {
        'name': 'Submission Assistant Agent',
        'description': 'Quality assessment, safety compliance, statistical review',
        'script': os.path.join(SKZ_AGENTS_DIR, 'skz-submission-assistant-agent.scm'),
        'capabilities': ['quality-assessment', 'safety-compliance', 'statistical-review'],
        'type': 'cognitive-agent', 
        'dependencies': ['skz-research-discovery'],
        'auto_restart': True
    },
    'skz-editorial-orchestration': {
        'name': 'Editorial Orchestration Agent',
        'description': 'Workflow coordination, decision making, conflict resolution',
        'script': os.path.join(SKZ_AGENTS_DIR, 'skz-editorial-orchestration-agent.scm'),
        'capabilities': ['workflow-coordination', 'decision-making', 'conflict-resolution'],
        'type': 'orchestrator-agent',
        'dependencies': ['skz-research-discovery', 'skz-submission-assistant'], 
        'auto_restart': True
    },
    'skz-atomspace-bridge': {
        'name': 'AtomSpace Bridge',
        'description': 'Bridge between SKZ agents and OpenCog AtomSpace',
        'script': os.path.join(BRIDGES_DIR, 'skz-atomspace-bridge.scm'),
        'capabilities': ['atomspace-integration', 'knowledge-representation'],
        'type': 'bridge-agent',
        'dependencies': [],
        'auto_restart': True
    }
}

class AgentManager:
    """Manages agent lifecycle and monitoring"""
    
    def __init__(self):
        self.agent_stats = {}
        self.monitoring_active = False
        self._init_stats()
    
    def _init_stats(self):
        """Initialize agent statistics"""
        for agent_id in AGENTS.keys():
            self.agent_stats[agent_id] = {
                'status': 'stopped',
                'pid': None,
                'start_time': None,
                'cpu_percent': 0.0,
                'memory_mb': 0.0,
                'restarts': 0,
                'last_error': None
            }
    
    def get_agent_status(self, agent_id):
        """Get current status of an agent"""
        if agent_id not in AGENTS:
            return None
        
        pid_file = os.path.join(PID_DIR, f"{agent_id}.pid")
        
        if os.path.exists(pid_file):
            try:
                with open(pid_file, 'r') as f:
                    pid = int(f.read().strip())
                
                # Check if process is running
                if psutil.pid_exists(pid):
                    process = psutil.Process(pid)
                    self.agent_stats[agent_id].update({
                        'status': 'running',
                        'pid': pid,
                        'cpu_percent': process.cpu_percent(),
                        'memory_mb': process.memory_info().rss / 1024 / 1024
                    })
                else:
                    # Process is dead but PID file exists
                    os.remove(pid_file)
                    self.agent_stats[agent_id]['status'] = 'stopped'
                    self.agent_stats[agent_id]['pid'] = None
            except (ValueError, PermissionError, FileNotFoundError):
                self.agent_stats[agent_id]['status'] = 'error'
        else:
            self.agent_stats[agent_id]['status'] = 'stopped'
            self.agent_stats[agent_id]['pid'] = None
        
        return self.agent_stats[agent_id]
    
    def start_agent(self, agent_id):
        """Start an agent"""
        if agent_id not in AGENTS:
            return {'error': f'Unknown agent: {agent_id}'}
        
        agent = AGENTS[agent_id]
        
        # Check if already running
        status = self.get_agent_status(agent_id)
        if status['status'] == 'running':
            return {'error': f'Agent {agent_id} is already running'}
        
        # Check script exists
        if not os.path.exists(agent['script']):
            return {'error': f'Agent script not found: {agent["script"]}'}
        
        try:
            # Make script executable
            os.chmod(agent['script'], 0o755)
            
            # Start agent
            log_file = os.path.join(LOG_DIR, f"{agent_id}.log")
            with open(log_file, 'a') as log:
                process = subprocess.Popen(
                    ['guile', agent['script'], '--register'],
                    stdout=log,
                    stderr=subprocess.STDOUT,
                    preexec_fn=os.setsid
                )
            
            # Save PID
            pid_file = os.path.join(PID_DIR, f"{agent_id}.pid")
            with open(pid_file, 'w') as f:
                f.write(str(process.pid))
            
            # Update stats
            self.agent_stats[agent_id].update({
                'status': 'starting',
                'pid': process.pid,
                'start_time': datetime.now().isoformat(),
                'last_error': None
            })
            
            logger.info(f"Started agent {agent_id} with PID {process.pid}")
            return {'success': f'Agent {agent_id} started successfully', 'pid': process.pid}
            
        except Exception as e:
            error_msg = f"Failed to start agent {agent_id}: {str(e)}"
            logger.error(error_msg)
            self.agent_stats[agent_id]['last_error'] = str(e)
            return {'error': error_msg}
    
    def stop_agent(self, agent_id):
        """Stop an agent"""
        if agent_id not in AGENTS:
            return {'error': f'Unknown agent: {agent_id}'}
        
        status = self.get_agent_status(agent_id)
        if status['status'] != 'running':
            return {'error': f'Agent {agent_id} is not running'}
        
        try:
            pid = status['pid']
            # Send SIGTERM to process group
            os.killpg(os.getpgid(pid), signal.SIGTERM)
            
            # Wait a bit then force kill if needed
            time.sleep(2)
            if psutil.pid_exists(pid):
                os.killpg(os.getpgid(pid), signal.SIGKILL)
            
            # Remove PID file
            pid_file = os.path.join(PID_DIR, f"{agent_id}.pid")
            if os.path.exists(pid_file):
                os.remove(pid_file)
            
            # Update stats
            self.agent_stats[agent_id].update({
                'status': 'stopped',
                'pid': None
            })
            
            logger.info(f"Stopped agent {agent_id}")
            return {'success': f'Agent {agent_id} stopped successfully'}
            
        except Exception as e:
            error_msg = f"Failed to stop agent {agent_id}: {str(e)}"
            logger.error(error_msg)
            return {'error': error_msg}
    
    def restart_agent(self, agent_id):
        """Restart an agent"""
        stop_result = self.stop_agent(agent_id)
        if 'error' in stop_result and 'not running' not in stop_result['error']:
            return stop_result
        
        time.sleep(1)  # Brief pause
        return self.start_agent(agent_id)
    
    def get_agent_logs(self, agent_id, lines=50):
        """Get recent log entries for an agent"""
        if agent_id not in AGENTS:
            return {'error': f'Unknown agent: {agent_id}'}
        
        log_file = os.path.join(LOG_DIR, f"{agent_id}.log")
        if not os.path.exists(log_file):
            return {'logs': []}
        
        try:
            with open(log_file, 'r') as f:
                all_lines = f.readlines()
                recent_lines = all_lines[-lines:] if len(all_lines) > lines else all_lines
                return {'logs': [line.strip() for line in recent_lines]}
        except Exception as e:
            return {'error': f'Failed to read logs: {str(e)}'}

# Global agent manager instance
agent_manager = AgentManager()

# REST API Routes

@app.route('/')
def dashboard():
    """Serve the agent management dashboard"""
    return send_from_directory(STATIC_DIR, 'dashboard.html')

@app.route('/static/<path:filename>')
def static_files(filename):
    """Serve static files"""
    return send_from_directory(STATIC_DIR, filename)

@app.route('/api/agents', methods=['GET'])
def list_agents():
    """List all available agents with their current status"""
    agents_status = {}
    for agent_id, agent_config in AGENTS.items():
        status = agent_manager.get_agent_status(agent_id)
        agents_status[agent_id] = {
            'config': agent_config,
            'status': status
        }
    return jsonify(agents_status)

@app.route('/api/agents/<agent_id>', methods=['GET'])
def get_agent(agent_id):
    """Get detailed information about a specific agent"""
    if agent_id not in AGENTS:
        return jsonify({'error': f'Agent {agent_id} not found'}), 404
    
    status = agent_manager.get_agent_status(agent_id)
    return jsonify({
        'config': AGENTS[agent_id],
        'status': status
    })

@app.route('/api/agents/<agent_id>/start', methods=['POST'])
def start_agent(agent_id):
    """Start an agent"""
    result = agent_manager.start_agent(agent_id)
    if 'error' in result:
        return jsonify(result), 400
    return jsonify(result)

@app.route('/api/agents/<agent_id>/stop', methods=['POST'])
def stop_agent(agent_id):
    """Stop an agent"""
    result = agent_manager.stop_agent(agent_id)
    if 'error' in result:
        return jsonify(result), 400
    return jsonify(result)

@app.route('/api/agents/<agent_id>/restart', methods=['POST'])
def restart_agent(agent_id):
    """Restart an agent"""
    result = agent_manager.restart_agent(agent_id)
    if 'error' in result:
        return jsonify(result), 400
    return jsonify(result)

@app.route('/api/agents/<agent_id>/logs', methods=['GET'])
def get_agent_logs(agent_id):
    """Get logs for an agent"""
    lines = request.args.get('lines', 50, type=int)
    result = agent_manager.get_agent_logs(agent_id, lines)
    if 'error' in result:
        return jsonify(result), 400
    return jsonify(result)

@app.route('/api/system/status', methods=['GET'])
def system_status():
    """Get overall system status"""
    total_agents = len(AGENTS)
    running_agents = sum(1 for agent_id in AGENTS.keys() 
                        if agent_manager.get_agent_status(agent_id)['status'] == 'running')
    
    # System metrics
    cpu_percent = psutil.cpu_percent(interval=1)
    memory = psutil.virtual_memory()
    disk = psutil.disk_usage('/')
    
    return jsonify({
        'agents': {
            'total': total_agents,
            'running': running_agents,
            'stopped': total_agents - running_agents
        },
        'system': {
            'cpu_percent': cpu_percent,
            'memory_percent': memory.percent,
            'memory_available_mb': memory.available / 1024 / 1024,
            'disk_percent': disk.percent,
            'disk_free_gb': disk.free / 1024 / 1024 / 1024
        },
        'timestamp': datetime.now().isoformat()
    })

@app.route('/api/operations/start-all', methods=['POST'])
def start_all_agents():
    """Start all agents in dependency order"""
    results = []
    
    # Start agents in dependency order
    start_order = ['skz-atomspace-bridge', 'skz-research-discovery', 
                   'skz-submission-assistant', 'skz-editorial-orchestration']
    
    for agent_id in start_order:
        if agent_id in AGENTS:
            result = agent_manager.start_agent(agent_id)
            results.append({'agent': agent_id, 'result': result})
            if 'success' in result:
                time.sleep(1)  # Brief pause between starts
    
    return jsonify({'operations': results})

@app.route('/api/operations/stop-all', methods=['POST'])
def stop_all_agents():
    """Stop all agents in reverse dependency order"""
    results = []
    
    # Stop agents in reverse dependency order
    stop_order = ['skz-editorial-orchestration', 'skz-submission-assistant',
                  'skz-research-discovery', 'skz-atomspace-bridge']
    
    for agent_id in stop_order:
        if agent_id in AGENTS:
            result = agent_manager.stop_agent(agent_id)
            results.append({'agent': agent_id, 'result': result})
    
    return jsonify({'operations': results})

if __name__ == '__main__':
    logger.info("Starting Agent Management API server...")
    logger.info(f"Dashboard available at: http://localhost:5002/")
    logger.info(f"API endpoints available at: http://localhost:5002/api/")
    
    # Start background monitoring
    def monitor_agents():
        while True:
            for agent_id in AGENTS.keys():
                agent_manager.get_agent_status(agent_id)
            time.sleep(5)
    
    monitor_thread = threading.Thread(target=monitor_agents, daemon=True)
    monitor_thread.start()
    
    # Start Flask app
    app.run(host='0.0.0.0', port=5002, debug=False, threaded=True)