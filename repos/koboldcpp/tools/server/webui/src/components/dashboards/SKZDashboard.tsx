import { useEffect, useState } from 'react';
import { Link } from 'react-router';
import {
  ChartBarIcon,
  DocumentTextIcon,
  UserGroupIcon,
  CogIcon,
  BeakerIcon,
  PrinterIcon,
  ChartPieIcon,
  ExclamationTriangleIcon,
} from '@heroicons/react/24/outline';
import { AgentStatus, skzApi, mockData } from '../../utils/skz-api';
import { classNames } from '../../utils/misc';

interface DashboardCardProps {
  agent: AgentStatus;
  icon: React.ComponentType<{ className?: string }>;
  description: string;
  route: string;
}

function DashboardCard({
  agent,
  icon: Icon,
  description,
  route,
}: DashboardCardProps) {
  const statusColors = {
    active: 'text-success',
    idle: 'text-warning',
    error: 'text-error',
    offline: 'text-base-content opacity-50',
  };

  const statusBg = {
    active: 'bg-success/10 border-success/20',
    idle: 'bg-warning/10 border-warning/20',
    error: 'bg-error/10 border-error/20',
    offline: 'bg-base-300 border-base-300',
  };

  return (
    <Link to={route} className="block">
      <div
        className={classNames({
          'card bg-base-100 shadow-md hover:shadow-lg transition-all duration-200 border-2':
            true,
          [statusBg[agent.status]]: true,
        })}
      >
        <div className="card-body p-6">
          <div className="flex items-start justify-between">
            <div className="flex items-center space-x-3">
              <Icon className="h-8 w-8 text-primary" />
              <div>
                <h3 className="card-title text-lg">{agent.name}</h3>
                <p className="text-sm text-base-content/70 mt-1">
                  {description}
                </p>
              </div>
            </div>
            <div className="flex flex-col items-end">
              <div
                className={classNames({
                  'badge badge-sm font-medium': true,
                  [statusColors[agent.status]]: true,
                })}
              >
                {agent.status}
              </div>
              {agent.lastUpdate && (
                <span className="text-xs text-base-content/50 mt-1">
                  {new Date(agent.lastUpdate).toLocaleTimeString()}
                </span>
              )}
            </div>
          </div>

          {agent.metrics && (
            <div className="mt-4 flex flex-wrap gap-2">
              {Object.entries(agent.metrics)
                .slice(0, 3)
                .map(([key, value]) => (
                  <div key={key} className="badge badge-outline badge-sm">
                    {key}: {value}
                  </div>
                ))}
            </div>
          )}
        </div>
      </div>
    </Link>
  );
}

export default function SKZDashboard() {
  const [agents, setAgents] = useState<AgentStatus[]>(mockData.agentStatus);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const fetchAgents = async () => {
      setLoading(true);
      try {
        const response = await skzApi.getAllAgentsStatus();
        if (response.success && response.data) {
          setAgents(response.data);
        } else {
          // Use mock data as fallback
          console.warn('Failed to fetch agent status, using mock data');
          setAgents(mockData.agentStatus);
          setError('Using demo data - connect to SKZ agents for live status');
        }
      } catch (err) {
        console.error('Error fetching agent status:', err);
        setAgents(mockData.agentStatus);
        setError('Using demo data - connect to SKZ agents for live status');
      } finally {
        setLoading(false);
      }
    };

    fetchAgents();

    // Set up periodic refresh
    const interval = setInterval(fetchAgents, 30000); // Refresh every 30 seconds

    return () => clearInterval(interval);
  }, []);

  const agentConfigs = [
    {
      id: 'research-discovery',
      icon: BeakerIcon,
      description:
        'INCI database mining, patent analysis, and trend identification',
      route: '/dashboard/research-discovery',
    },
    {
      id: 'submission-assistant',
      icon: DocumentTextIcon,
      description:
        'Quality assessment, safety compliance, and statistical review',
      route: '/dashboard/submission-assistant',
    },
    {
      id: 'editorial-orchestration',
      icon: CogIcon,
      description:
        'Workflow coordination, decision making, and conflict resolution',
      route: '/dashboard/editorial-orchestration',
    },
    {
      id: 'review-coordination',
      icon: UserGroupIcon,
      description:
        'Reviewer matching, workload management, and quality monitoring',
      route: '/dashboard/review-coordination',
    },
    {
      id: 'content-quality',
      icon: ChartBarIcon,
      description:
        'Scientific validation, safety assessment, and standards enforcement',
      route: '/dashboard/content-quality',
    },
    {
      id: 'publishing-production',
      icon: PrinterIcon,
      description:
        'Content formatting, visual generation, and multi-channel distribution',
      route: '/dashboard/publishing-production',
    },
    {
      id: 'analytics-monitoring',
      icon: ChartPieIcon,
      description:
        'Performance analytics, trend forecasting, and strategic insights',
      route: '/dashboard/analytics-monitoring',
    },
  ];

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-96">
        <div className="loading loading-spinner loading-lg text-primary"></div>
        <span className="ml-4 text-lg">Loading SKZ agents...</span>
      </div>
    );
  }

  const activeAgents = agents.filter((a) => a.status === 'active').length;
  const errorAgents = agents.filter((a) => a.status === 'error').length;

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="text-center space-y-2">
        <h1 className="text-3xl font-bold text-base-content">
          SKZ Autonomous Agents Dashboard
        </h1>
        <p className="text-base-content/70 max-w-2xl mx-auto">
          Monitor and control the 7 autonomous agents powering the OpenCog
          cognitive ecosystem
        </p>
      </div>

      {/* Status Overview */}
      <div className="stats shadow w-full bg-base-100">
        <div className="stat">
          <div className="stat-figure text-success">
            <CogIcon className="w-8 h-8" />
          </div>
          <div className="stat-title">Active Agents</div>
          <div className="stat-value text-success">{activeAgents}</div>
          <div className="stat-desc">out of {agents.length} total</div>
        </div>

        <div className="stat">
          <div className="stat-figure text-warning">
            <ChartBarIcon className="w-8 h-8" />
          </div>
          <div className="stat-title">System Load</div>
          <div className="stat-value text-warning">
            {Math.round((activeAgents / agents.length) * 100)}%
          </div>
          <div className="stat-desc">Cognitive processing active</div>
        </div>

        <div className="stat">
          <div className="stat-figure text-error">
            <ExclamationTriangleIcon className="w-8 h-8" />
          </div>
          <div className="stat-title">Issues</div>
          <div className="stat-value text-error">{errorAgents}</div>
          <div className="stat-desc">Agents need attention</div>
        </div>
      </div>

      {/* Error Alert */}
      {error && (
        <div className="alert alert-warning">
          <ExclamationTriangleIcon className="w-6 h-6" />
          <span>{error}</span>
        </div>
      )}

      {/* Agent Cards Grid */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
        {agentConfigs.map((config) => {
          const agent = agents.find((a) => a.id === config.id) || {
            id: config.id,
            name:
              config.id
                .split('-')
                .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
                .join(' ') + ' Agent',
            status: 'offline' as const,
            lastUpdate: new Date().toISOString(),
          };

          return (
            <DashboardCard
              key={config.id}
              agent={agent}
              icon={config.icon}
              description={config.description}
              route={config.route}
            />
          );
        })}
      </div>

      {/* Quick Actions */}
      <div className="card bg-base-100 shadow-md">
        <div className="card-body">
          <h2 className="card-title mb-4">Quick Actions</h2>
          <div className="flex flex-wrap gap-3">
            <button className="btn btn-primary btn-sm">
              <CogIcon className="w-4 h-4" />
              Start All Agents
            </button>
            <button className="btn btn-secondary btn-sm">
              <ChartBarIcon className="w-4 h-4" />
              System Health Check
            </button>
            <button className="btn btn-accent btn-sm">
              <DocumentTextIcon className="w-4 h-4" />
              Export Reports
            </button>
            <Link
              to="/dashboard/analytics-monitoring"
              className="btn btn-outline btn-sm"
            >
              <ChartPieIcon className="w-4 h-4" />
              View Analytics
            </Link>
          </div>
        </div>
      </div>

      {/* Footer */}
      <div className="text-center text-sm text-base-content/50 py-4">
        SKZ Autonomous Agents Framework integrated with OpenCog Cognitive
        Ecosystem
        <br />
        <span className="text-xs">
          Phase 3: Frontend Integration - Real-time dashboards powered by React
          & KoboldCpp
        </span>
      </div>
    </div>
  );
}
