import { useEffect, useState } from 'react';
import { Link } from 'react-router';
import {
  DocumentTextIcon,
  CheckCircleIcon,
  ExclamationTriangleIcon,
  ClockIcon,
  ArrowLeftIcon,
  ChartBarIcon,
} from '@heroicons/react/24/outline';
import { SubmissionData, skzApi, WebSocketMessage } from '../../utils/skz-api';
import { classNames } from '../../utils/misc';

export default function SubmissionAssistantDashboard() {
  const [data, setData] = useState<SubmissionData | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const fetchData = async () => {
      setLoading(true);
      try {
        const response = await skzApi.getSubmissionData();
        if (response.success && response.data) {
          setData(response.data);
        } else {
          // Mock data for demonstration
          setData({
            submissions: [
              {
                id: 'SUB001',
                title: 'Novel Retinol Formulation Study',
                status: 'reviewing',
                qualityScore: 8.5,
                complianceStatus: 'compliant',
              },
              {
                id: 'SUB002',
                title: 'Peptide Anti-Aging Research',
                status: 'pending',
                qualityScore: 7.2,
                complianceStatus: 'issues',
              },
              {
                id: 'SUB003',
                title: 'Vitamin C Stability Analysis',
                status: 'approved',
                qualityScore: 9.1,
                complianceStatus: 'compliant',
              },
            ],
            stats: {
              totalSubmissions: 127,
              avgProcessingTime: 5.2,
              approvalRate: 78.3,
            },
          });
          setError(
            'Using demo data - connect to SKZ Submission Assistant for live data'
          );
        }
      } catch (err) {
        console.error('Error fetching submission data:', err);
        setError('Failed to load data');
      } finally {
        setLoading(false);
      }
    };

    fetchData();

    // WebSocket for real-time updates
    skzApi.connectWebSocket(
      'submission-assistant',
      (message: WebSocketMessage) => {
        if (message.type === 'submission_update' && message.data) {
          setData(message.data as SubmissionData);
        }
      }
    );

    return () => skzApi.disconnectWebSocket('submission-assistant');
  }, []);

  if (loading && !data) {
    return (
      <div className="flex items-center justify-center min-h-96">
        <div className="loading loading-spinner loading-lg text-primary"></div>
        <span className="ml-4 text-lg">Loading submission data...</span>
      </div>
    );
  }

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'approved':
        return 'text-success';
      case 'rejected':
        return 'text-error';
      case 'reviewing':
        return 'text-info';
      default:
        return 'text-warning';
    }
  };

  const getComplianceColor = (status: string) => {
    switch (status) {
      case 'compliant':
        return 'text-success';
      case 'issues':
        return 'text-error';
      default:
        return 'text-warning';
    }
  };

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
            <DocumentTextIcon className="w-8 h-8 text-primary" />
            <div>
              <h1 className="text-2xl font-bold">Submission Assistant Agent</h1>
              <p className="text-base-content/70">
                Quality assessment, safety compliance, and statistical review
              </p>
            </div>
          </div>
        </div>
        <div className="badge badge-success badge-lg">Active</div>
      </div>

      {/* Error Alert */}
      {error && (
        <div className="alert alert-warning">
          <span>{error}</span>
        </div>
      )}

      {/* Statistics */}
      <div className="stats shadow w-full bg-base-100">
        <div className="stat">
          <div className="stat-figure text-primary">
            <DocumentTextIcon className="w-8 h-8" />
          </div>
          <div className="stat-title">Total Submissions</div>
          <div className="stat-value text-primary">
            {data?.stats.totalSubmissions || 0}
          </div>
          <div className="stat-desc">Processed by agent</div>
        </div>

        <div className="stat">
          <div className="stat-figure text-secondary">
            <ClockIcon className="w-8 h-8" />
          </div>
          <div className="stat-title">Avg Processing Time</div>
          <div className="stat-value text-secondary">
            {data?.stats.avgProcessingTime || 0} days
          </div>
          <div className="stat-desc">Per submission</div>
        </div>

        <div className="stat">
          <div className="stat-figure text-accent">
            <CheckCircleIcon className="w-8 h-8" />
          </div>
          <div className="stat-title">Approval Rate</div>
          <div className="stat-value text-accent">
            {data?.stats.approvalRate || 0}%
          </div>
          <div className="stat-desc">Quality assessments passed</div>
        </div>
      </div>

      {/* Recent Submissions */}
      <div className="card bg-base-100 shadow-md">
        <div className="card-body">
          <h2 className="card-title mb-4">Recent Submissions</h2>
          <div className="overflow-x-auto">
            <table className="table table-zebra">
              <thead>
                <tr>
                  <th>ID</th>
                  <th>Title</th>
                  <th>Status</th>
                  <th>Quality Score</th>
                  <th>Compliance</th>
                  <th>Actions</th>
                </tr>
              </thead>
              <tbody>
                {data?.submissions?.map((submission, index) => (
                  <tr key={index}>
                    <td className="font-mono">{submission.id}</td>
                    <td className="max-w-xs truncate">{submission.title}</td>
                    <td>
                      <div
                        className={classNames({
                          'badge badge-sm': true,
                          [getStatusColor(submission.status)]: true,
                        })}
                      >
                        {submission.status}
                      </div>
                    </td>
                    <td>
                      <div className="flex items-center space-x-2">
                        <progress
                          className={classNames({
                            'progress w-20': true,
                            'progress-success': submission.qualityScore >= 8,
                            'progress-warning':
                              submission.qualityScore >= 6 &&
                              submission.qualityScore < 8,
                            'progress-error': submission.qualityScore < 6,
                          })}
                          value={submission.qualityScore}
                          max="10"
                        />
                        <span className="text-sm">
                          {submission.qualityScore}/10
                        </span>
                      </div>
                    </td>
                    <td>
                      <span
                        className={classNames({
                          'badge badge-sm': true,
                          [getComplianceColor(submission.complianceStatus)]:
                            true,
                        })}
                      >
                        {submission.complianceStatus}
                      </span>
                    </td>
                    <td>
                      <div className="flex space-x-1">
                        <button className="btn btn-xs btn-outline">
                          Review
                        </button>
                        <button className="btn btn-xs btn-outline">
                          Assess
                        </button>
                      </div>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      </div>

      {/* Quick Actions */}
      <div className="card bg-base-100 shadow-md">
        <div className="card-body">
          <h2 className="card-title mb-4">Agent Actions</h2>
          <div className="flex flex-wrap gap-2">
            <button className="btn btn-primary btn-sm">
              <DocumentTextIcon className="w-4 h-4" />
              Start Batch Assessment
            </button>
            <button className="btn btn-secondary btn-sm">
              <ChartBarIcon className="w-4 h-4" />
              Quality Report
            </button>
            <button className="btn btn-accent btn-sm">
              <CheckCircleIcon className="w-4 h-4" />
              Compliance Check
            </button>
            <button className="btn btn-outline btn-sm">
              <ExclamationTriangleIcon className="w-4 h-4" />
              Review Issues
            </button>
          </div>
        </div>
      </div>
    </div>
  );
}
