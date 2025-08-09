import { useEffect, useState } from 'react';
import { Link } from 'react-router';
import {
  ChartPieIcon,
  ArrowTrendingUpIcon,
  ClockIcon,
  ExclamationTriangleIcon,
  ArrowLeftIcon,
  ChartBarIcon,
  EyeIcon,
} from '@heroicons/react/24/outline';
import { AnalyticsData, skzApi, WebSocketMessage } from '../../utils/skz-api';
import { classNames } from '../../utils/misc';

export default function AnalyticsMonitoringDashboard() {
  const [data, setData] = useState<AnalyticsData | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [connectionStatus, setConnectionStatus] =
    useState<string>('connecting');

  useEffect(() => {
    const fetchData = async () => {
      setLoading(true);
      try {
        const response = await skzApi.getAnalyticsData();
        if (response.success && response.data) {
          setData(response.data);
        } else {
          // Mock data for demonstration
          setData({
            performance: {
              avgProcessingTime: 4.2,
              throughput: 156,
              errorRate: 2.1,
              satisfaction: 4.6,
            },
            trends: [
              {
                metric: 'Processing Speed',
                value: 142,
                change: 12.5,
                period: 'Last 30 days',
              },
              {
                metric: 'Quality Score',
                value: 8.7,
                change: -2.1,
                period: 'Last 30 days',
              },
              {
                metric: 'User Satisfaction',
                value: 4.6,
                change: 8.3,
                period: 'Last 30 days',
              },
            ],
            forecasts: [
              {
                metric: 'Throughput',
                prediction: 185,
                confidence: 87,
                timeframe: 'Next month',
              },
              {
                metric: 'Quality',
                prediction: 9.1,
                confidence: 92,
                timeframe: 'Next month',
              },
            ],
          });
          setError(
            'Using demo data - connect to SKZ Analytics Agent for live data'
          );
        }
      } catch (err) {
        console.error('Error fetching analytics data:', err);
        setError('Failed to load data');
      } finally {
        setLoading(false);
      }
    };

    fetchData();

    // WebSocket for real-time updates
    skzApi.connectWebSocket(
      'analytics-monitoring',
      (message: WebSocketMessage) => {
        if (message.type === 'analytics_update' && message.data) {
          setData(message.data as AnalyticsData);
        }
      },
      true // Enable auto-reconnect
    );

    // Update connection status periodically
    const updateConnectionStatus = () => {
      const status = skzApi.getConnectionStatus('analytics-monitoring');
      setConnectionStatus(status?.status || 'disconnected');
    };

    updateConnectionStatus();
    const statusInterval = setInterval(updateConnectionStatus, 2000);

    return () => {
      skzApi.disconnectWebSocket('analytics-monitoring');
      clearInterval(statusInterval);
    };
  }, []);

  // Log connection status for debugging (satisfies TypeScript unused variable warning)
  if (process.env.NODE_ENV === 'development') {
    console.log(`Analytics dashboard connection status: ${connectionStatus}`);
  }

  if (loading && !data) {
    return (
      <div className="flex items-center justify-center min-h-96">
        <div className="loading loading-spinner loading-lg text-primary"></div>
        <span className="ml-4 text-lg">Loading analytics data...</span>
      </div>
    );
  }

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
            <ChartPieIcon className="w-8 h-8 text-primary" />
            <div>
              <h1 className="text-2xl font-bold">
                Analytics & Monitoring Agent
              </h1>
              <p className="text-base-content/70">
                Performance analytics, trend forecasting, and strategic insights
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

      {/* Performance Metrics */}
      <div className="stats shadow w-full bg-base-100">
        <div className="stat">
          <div className="stat-figure text-primary">
            <ClockIcon className="w-8 h-8" />
          </div>
          <div className="stat-title">Avg Processing Time</div>
          <div className="stat-value text-primary">
            {data?.performance.avgProcessingTime}d
          </div>
          <div className="stat-desc">Per submission</div>
        </div>

        <div className="stat">
          <div className="stat-figure text-secondary">
            <ArrowTrendingUpIcon className="w-8 h-8" />
          </div>
          <div className="stat-title">Daily Throughput</div>
          <div className="stat-value text-secondary">
            {data?.performance.throughput}
          </div>
          <div className="stat-desc">Submissions processed</div>
        </div>

        <div className="stat">
          <div className="stat-figure text-error">
            <ExclamationTriangleIcon className="w-8 h-8" />
          </div>
          <div className="stat-title">Error Rate</div>
          <div className="stat-value text-error">
            {data?.performance.errorRate}%
          </div>
          <div className="stat-desc">System reliability</div>
        </div>

        <div className="stat">
          <div className="stat-figure text-accent">
            <EyeIcon className="w-8 h-8" />
          </div>
          <div className="stat-title">Satisfaction</div>
          <div className="stat-value text-accent">
            {data?.performance.satisfaction}/5
          </div>
          <div className="stat-desc">User rating</div>
        </div>
      </div>

      {/* Trends Analysis */}
      <div className="card bg-base-100 shadow-md">
        <div className="card-body">
          <h2 className="card-title mb-4">
            <ArrowTrendingUpIcon className="w-5 h-5" />
            Performance Trends
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            {data?.trends?.map((trend, index) => (
              <div key={index} className="stat bg-base-200 rounded-lg">
                <div className="stat-title">{trend.metric}</div>
                <div className="stat-value">{trend.value}</div>
                <div
                  className={classNames({
                    'stat-desc': true,
                    'text-success': trend.change > 0,
                    'text-error': trend.change < 0,
                    'text-warning': trend.change === 0,
                  })}
                >
                  {trend.change > 0 ? '+' : ''}
                  {trend.change}% {trend.period}
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>

      {/* Forecasting */}
      <div className="card bg-base-100 shadow-md">
        <div className="card-body">
          <h2 className="card-title mb-4">
            <ChartBarIcon className="w-5 h-5" />
            Predictive Forecasting
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            {data?.forecasts?.map((forecast, index) => (
              <div key={index} className="card bg-base-200 shadow-sm">
                <div className="card-body p-4">
                  <h3 className="font-semibold mb-2">{forecast.metric}</h3>
                  <div className="flex items-center justify-between">
                    <div>
                      <div className="text-2xl font-bold">
                        {forecast.prediction}
                      </div>
                      <div className="text-sm text-base-content/70">
                        {forecast.timeframe}
                      </div>
                    </div>
                    <div className="text-right">
                      <div className="text-sm">Confidence</div>
                      <div className="font-semibold">
                        {forecast.confidence}%
                      </div>
                    </div>
                  </div>
                  <progress
                    className="progress progress-primary w-full mt-2"
                    value={forecast.confidence}
                    max="100"
                  />
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>

      {/* Quick Actions */}
      <div className="card bg-base-100 shadow-md">
        <div className="card-body">
          <h2 className="card-title mb-4">Agent Actions</h2>
          <div className="flex flex-wrap gap-2">
            <button className="btn btn-primary btn-sm">
              <ChartBarIcon className="w-4 h-4" />
              Generate Report
            </button>
            <button className="btn btn-secondary btn-sm">
              <ArrowTrendingUpIcon className="w-4 h-4" />
              Trend Analysis
            </button>
            <button className="btn btn-accent btn-sm">
              <EyeIcon className="w-4 h-4" />
              Performance Review
            </button>
            <button className="btn btn-outline btn-sm">
              <ExclamationTriangleIcon className="w-4 h-4" />
              Alert Settings
            </button>
          </div>
        </div>
      </div>
    </div>
  );
}
