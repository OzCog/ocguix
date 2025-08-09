import { Link } from 'react-router';
import { ArrowLeftIcon, CogIcon } from '@heroicons/react/24/outline';

interface PlaceholderDashboardProps {
  agentName: string;
  description: string;
  icon: React.ComponentType<{ className?: string }>;
}

export default function PlaceholderDashboard({
  agentName,
  description,
  icon: Icon,
}: PlaceholderDashboardProps) {
  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div className="flex items-center space-x-4">
          <Link to="/dashboard" className="btn btn-ghost btn-sm">
            <ArrowLeftIcon className="w-4 h-4" />
            Back to Dashboard
          </Link>
          <div className="flex items-center space-x-3">
            <Icon className="w-8 h-8 text-primary" />
            <div>
              <h1 className="text-2xl font-bold">{agentName}</h1>
              <p className="text-base-content/70">{description}</p>
            </div>
          </div>
        </div>
        <div className="badge badge-warning badge-lg">Coming Soon</div>
      </div>

      {/* Placeholder Content */}
      <div className="card bg-base-100 shadow-md">
        <div className="card-body text-center py-16">
          <Icon className="w-24 h-24 text-base-content/30 mx-auto mb-6" />
          <h2 className="text-3xl font-bold mb-4">
            Dashboard Under Development
          </h2>
          <p className="text-base-content/70 mb-6 max-w-2xl mx-auto">
            This agent dashboard is currently being developed. The {agentName}{' '}
            will provide {description.toLowerCase()} capabilities as part of the
            SKZ autonomous agents framework.
          </p>
          <div className="flex justify-center space-x-4">
            <Link to="/dashboard" className="btn btn-primary">
              Back to Main Dashboard
            </Link>
            <button className="btn btn-outline" disabled>
              <CogIcon className="w-4 h-4" />
              Configure Agent
            </button>
          </div>
        </div>
      </div>

      {/* Development Status */}
      <div className="card bg-base-100 shadow-md">
        <div className="card-body">
          <h3 className="card-title mb-4">Development Status</h3>
          <div className="space-y-3">
            <div className="flex items-center justify-between">
              <span>Agent Framework</span>
              <div className="badge badge-success">Complete</div>
            </div>
            <div className="flex items-center justify-between">
              <span>API Integration</span>
              <div className="badge badge-success">Complete</div>
            </div>
            <div className="flex items-center justify-between">
              <span>Dashboard UI</span>
              <div className="badge badge-warning">In Progress</div>
            </div>
            <div className="flex items-center justify-between">
              <span>Real-time Updates</span>
              <div className="badge badge-info">Planned</div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
