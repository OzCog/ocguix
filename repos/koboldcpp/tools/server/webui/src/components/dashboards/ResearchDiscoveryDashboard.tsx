import { useEffect, useState } from 'react';
import { Link } from 'react-router';
import {
  BeakerIcon,
  ArrowTrendingUpIcon,
  ArrowTrendingDownIcon,
  MinusIcon,
  DocumentTextIcon,
  ChartBarIcon,
  MagnifyingGlassIcon,
  ArrowLeftIcon,
  SignalIcon,
  WifiIcon,
  ExclamationCircleIcon,
} from '@heroicons/react/24/outline';
import {
  ResearchData,
  skzApi,
  mockData,
  WebSocketMessage,
} from '../../utils/skz-api';
import { classNames } from '../../utils/misc';

export default function ResearchDiscoveryDashboard() {
  const [data, setData] = useState<ResearchData | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [searchQuery, setSearchQuery] = useState('');
  const [connectionStatus, setConnectionStatus] =
    useState<string>('connecting');

  useEffect(() => {
    const fetchData = async () => {
      setLoading(true);
      try {
        const response = await skzApi.getResearchData();
        if (response.success && response.data) {
          setData(response.data);
        } else {
          setData(mockData.researchData);
          setError(
            'Using demo data - connect to SKZ Research Discovery Agent for live data'
          );
        }
      } catch (err) {
        console.error('Error fetching research data:', err);
        setData(mockData.researchData);
        setError(
          'Using demo data - connect to SKZ Research Discovery Agent for live data'
        );
      } finally {
        setLoading(false);
      }
    };

    fetchData();

    // WebSocket for real-time updates
    const handleWebSocketMessage = (message: WebSocketMessage) => {
      if (message.type === 'research_update' && message.data) {
        setData(message.data as ResearchData);
      }
    };

    skzApi.connectWebSocket('research-discovery', handleWebSocketMessage, true);

    // Update connection status periodically
    const updateConnectionStatus = () => {
      const status = skzApi.getConnectionStatus('research-discovery');
      setConnectionStatus(status?.status || 'disconnected');
    };

    updateConnectionStatus();
    const statusInterval = setInterval(updateConnectionStatus, 2000);

    return () => {
      skzApi.disconnectWebSocket('research-discovery');
      clearInterval(statusInterval);
    };
  }, []);

  const handlePatentSearch = async () => {
    if (!searchQuery.trim()) return;

    setLoading(true);
    try {
      const response = await skzApi.searchPatents(searchQuery);
      if (response.success && response.data) {
        // Update patent data with search results
        console.log('Patent search results:', response.data);
      }
    } catch (err) {
      console.error('Patent search error:', err);
    } finally {
      setLoading(false);
    }
  };

  if (loading && !data) {
    return (
      <div className="flex items-center justify-center min-h-96">
        <div className="loading loading-spinner loading-lg text-primary"></div>
        <span className="ml-4 text-lg">Loading research data...</span>
      </div>
    );
  }

  const getTrendIcon = (trend: 'up' | 'down' | 'stable') => {
    switch (trend) {
      case 'up':
        return <ArrowTrendingUpIcon className="w-4 h-4 text-success" />;
      case 'down':
        return <ArrowTrendingDownIcon className="w-4 h-4 text-error" />;
      default:
        return <MinusIcon className="w-4 h-4 text-warning" />;
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
            <BeakerIcon className="w-8 h-8 text-primary" />
            <div>
              <h1 className="text-2xl font-bold">Research Discovery Agent</h1>
              <p className="text-base-content/70">
                INCI database mining, patent analysis, and trend identification
              </p>
            </div>
          </div>
        </div>
        <div className="flex items-center justify-between">
          <div className="badge badge-success badge-lg">Active</div>
          <div className="flex items-center gap-2 text-sm">
            {connectionStatus === 'connected' && (
              <>
                <SignalIcon className="w-4 h-4 text-success" />
                <span className="text-success">Real-time updates active</span>
              </>
            )}
            {(connectionStatus === 'connecting' ||
              connectionStatus === 'reconnecting') && (
              <>
                <WifiIcon className="w-4 h-4 text-warning animate-pulse" />
                <span className="text-warning">Connecting...</span>
              </>
            )}
            {(connectionStatus === 'error' ||
              connectionStatus === 'disconnected') && (
              <>
                <ExclamationCircleIcon className="w-4 h-4 text-error" />
                <span className="text-error">
                  Connection lost - using cached data
                </span>
              </>
            )}
          </div>
        </div>
      </div>

      {/* Error Alert */}
      {error && (
        <div className="alert alert-warning">
          <span>{error}</span>
        </div>
      )}

      {/* Key Metrics */}
      <div className="stats shadow w-full bg-base-100">
        <div className="stat">
          <div className="stat-figure text-primary">
            <ChartBarIcon className="w-8 h-8" />
          </div>
          <div className="stat-title">Active Trends</div>
          <div className="stat-value text-primary">
            {data?.trends?.length || 0}
          </div>
          <div className="stat-desc">Trending keywords identified</div>
        </div>

        <div className="stat">
          <div className="stat-figure text-secondary">
            <DocumentTextIcon className="w-8 h-8" />
          </div>
          <div className="stat-title">Relevant Patents</div>
          <div className="stat-value text-secondary">
            {data?.patents?.length || 0}
          </div>
          <div className="stat-desc">High relevance patents found</div>
        </div>

        <div className="stat">
          <div className="stat-figure text-accent">
            <BeakerIcon className="w-8 h-8" />
          </div>
          <div className="stat-title">INCI Ingredients</div>
          <div className="stat-value text-accent">
            {data?.inciData?.length || 0}
          </div>
          <div className="stat-desc">Safety-assessed ingredients</div>
        </div>
      </div>

      {/* Patent Search */}
      <div className="card bg-base-100 shadow-md">
        <div className="card-body">
          <h2 className="card-title mb-4">
            <MagnifyingGlassIcon className="w-5 h-5" />
            Patent Search
          </h2>
          <div className="flex gap-2">
            <input
              type="text"
              className="input input-bordered flex-1"
              placeholder="Search patents by keyword, ingredient, or technology..."
              value={searchQuery}
              onChange={(e) => setSearchQuery(e.target.value)}
              onKeyPress={(e) => e.key === 'Enter' && handlePatentSearch()}
            />
            <button
              className="btn btn-primary"
              onClick={handlePatentSearch}
              disabled={!searchQuery.trim() || loading}
            >
              {loading ? (
                <span className="loading loading-spinner loading-sm"></span>
              ) : (
                'Search'
              )}
            </button>
          </div>
        </div>
      </div>

      {/* Trending Keywords */}
      <div className="card bg-base-100 shadow-md">
        <div className="card-body">
          <h2 className="card-title mb-4">
            <ArrowTrendingUpIcon className="w-5 h-5" />
            Trending Keywords
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
            {data?.trends?.map((trend, index) => (
              <div
                key={index}
                className="flex items-center justify-between p-4 bg-base-200 rounded-lg"
              >
                <div>
                  <div className="font-semibold">{trend.keyword}</div>
                  <div className="text-sm text-base-content/70">
                    {trend.count} mentions
                  </div>
                </div>
                <div className="flex items-center space-x-2">
                  {getTrendIcon(trend.trend)}
                  <span
                    className={classNames({
                      'text-sm font-medium': true,
                      'text-success': trend.trend === 'up',
                      'text-error': trend.trend === 'down',
                      'text-warning': trend.trend === 'stable',
                    })}
                  >
                    {trend.trend}
                  </span>
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>

      {/* Patent Analysis */}
      <div className="card bg-base-100 shadow-md">
        <div className="card-body">
          <h2 className="card-title mb-4">
            <DocumentTextIcon className="w-5 h-5" />
            Relevant Patents
          </h2>
          <div className="overflow-x-auto">
            <table className="table table-zebra">
              <thead>
                <tr>
                  <th>Patent ID</th>
                  <th>Title</th>
                  <th>Relevance</th>
                  <th>Actions</th>
                </tr>
              </thead>
              <tbody>
                {data?.patents?.map((patent, index) => (
                  <tr key={index}>
                    <td className="font-mono">{patent.id}</td>
                    <td className="max-w-xs truncate">{patent.title}</td>
                    <td>
                      <div className="flex items-center space-x-2">
                        <progress
                          className="progress progress-primary w-20"
                          value={patent.relevance * 100}
                          max="100"
                        />
                        <span className="text-sm">
                          {Math.round(patent.relevance * 100)}%
                        </span>
                      </div>
                    </td>
                    <td>
                      <div className="flex space-x-1">
                        <button className="btn btn-xs btn-outline">View</button>
                        <button className="btn btn-xs btn-outline">
                          Analyze
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

      {/* INCI Safety Data */}
      <div className="card bg-base-100 shadow-md">
        <div className="card-body">
          <h2 className="card-title mb-4">
            <BeakerIcon className="w-5 h-5" />
            INCI Safety Assessment
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            {data?.inciData?.map((ingredient, index) => (
              <div key={index} className="card bg-base-200 shadow-sm">
                <div className="card-body p-4">
                  <div className="flex items-center justify-between mb-2">
                    <h3 className="font-semibold">{ingredient.name}</h3>
                    <div className="badge badge-outline">
                      {ingredient.category}
                    </div>
                  </div>
                  <div className="flex items-center space-x-2">
                    <span className="text-sm">Safety Rating:</span>
                    <div className="flex items-center space-x-1">
                      <progress
                        className={classNames({
                          'progress w-20': true,
                          'progress-success': ingredient.safetyRating >= 8,
                          'progress-warning':
                            ingredient.safetyRating >= 5 &&
                            ingredient.safetyRating < 8,
                          'progress-error': ingredient.safetyRating < 5,
                        })}
                        value={ingredient.safetyRating}
                        max="10"
                      />
                      <span className="text-sm font-medium">
                        {ingredient.safetyRating}/10
                      </span>
                    </div>
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>

      {/* Actions */}
      <div className="card bg-base-100 shadow-md">
        <div className="card-body">
          <h2 className="card-title mb-4">Agent Actions</h2>
          <div className="flex flex-wrap gap-2">
            <button className="btn btn-primary btn-sm">
              <MagnifyingGlassIcon className="w-4 h-4" />
              Start New Research
            </button>
            <button className="btn btn-secondary btn-sm">
              <DocumentTextIcon className="w-4 h-4" />
              Export Findings
            </button>
            <button className="btn btn-accent btn-sm">
              <ChartBarIcon className="w-4 h-4" />
              Generate Report
            </button>
            <button className="btn btn-outline btn-sm">
              <ArrowTrendingUpIcon className="w-4 h-4" />
              Trend Analysis
            </button>
          </div>
        </div>
      </div>
    </div>
  );
}
