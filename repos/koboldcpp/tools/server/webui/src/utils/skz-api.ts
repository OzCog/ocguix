/**
 * SKZ Agents API Integration Layer
 * Provides TypeScript interface to communicate with SKZ autonomous agents framework
 */

export interface AgentStatus {
  id: string;
  name: string;
  status: 'active' | 'idle' | 'error' | 'offline';
  lastUpdate: string;
  metrics?: Record<string, string | number>;
}

export interface AgentResponse<T = unknown> {
  success: boolean;
  data?: T;
  error?: string;
  timestamp: string;
}

export interface ResearchData {
  trends: Array<{
    keyword: string;
    count: number;
    trend: 'up' | 'down' | 'stable';
  }>;
  patents: Array<{
    id: string;
    title: string;
    relevance: number;
  }>;
  inciData: Array<{
    name: string;
    category: string;
    safetyRating: number;
  }>;
}

export interface SubmissionData {
  submissions: Array<{
    id: string;
    title: string;
    status: 'pending' | 'reviewing' | 'approved' | 'rejected';
    qualityScore: number;
    complianceStatus: 'compliant' | 'issues' | 'pending';
  }>;
  stats: {
    totalSubmissions: number;
    avgProcessingTime: number;
    approvalRate: number;
  };
}

export interface EditorialData {
  workflows: Array<{
    id: string;
    name: string;
    status: 'active' | 'paused' | 'completed';
    progress: number;
    conflicts?: string[];
  }>;
  decisions: {
    pending: number;
    resolved: number;
    escalated: number;
  };
}

export interface ReviewData {
  reviewers: Array<{
    id: string;
    name: string;
    workload: number;
    expertise: string[];
    availability: 'available' | 'busy' | 'unavailable';
  }>;
  assignments: Array<{
    submissionId: string;
    reviewerId: string;
    dueDate: string;
    status: 'assigned' | 'in_progress' | 'completed';
  }>;
}

export interface QualityData {
  assessments: Array<{
    submissionId: string;
    scientificValidity: number;
    safetyCompliance: number;
    standardsAdherence: number;
    overallScore: number;
  }>;
  violations: Array<{
    type: string;
    severity: 'low' | 'medium' | 'high';
    description: string;
    submissionId: string;
  }>;
}

export interface PublishingData {
  pipeline: Array<{
    submissionId: string;
    stage: 'formatting' | 'visual_generation' | 'distribution' | 'completed';
    progress: number;
    estimatedCompletion: string;
  }>;
  channels: Array<{
    name: string;
    status: 'active' | 'maintenance' | 'error';
    lastPublication: string;
  }>;
}

export interface AnalyticsData {
  performance: {
    avgProcessingTime: number;
    throughput: number;
    errorRate: number;
    satisfaction: number;
  };
  trends: Array<{
    metric: string;
    value: number;
    change: number;
    period: string;
  }>;
  forecasts: Array<{
    metric: string;
    prediction: number;
    confidence: number;
    timeframe: string;
  }>;
}

// Additional type interfaces
export interface PatentSearchResult {
  id: string;
  title: string;
  relevance: number;
  abstract?: string;
}

export interface AssessmentResult {
  submissionId: string;
  qualityScore: number;
  complianceStatus: 'compliant' | 'issues' | 'pending';
  recommendations: string[];
}

export interface ConflictDecision {
  decision: 'approve' | 'reject' | 'escalate';
  reasoning: string;
  reviewerId?: string;
}

export interface ConflictResolution {
  workflowId: string;
  resolved: boolean;
  decision: string;
  timestamp: string;
}

export interface ReviewAssignment {
  submissionId: string;
  reviewerId: string;
  dueDate: string;
  assignmentId: string;
}

export interface ValidationResult {
  submissionId: string;
  validationPassed: boolean;
  issues: Array<{
    type: string;
    severity: 'low' | 'medium' | 'high';
    description: string;
  }>;
}

export interface PublicationResult {
  submissionId: string;
  publishedChannels: string[];
  publicationId: string;
  status: 'success' | 'partial' | 'failed';
}

export interface WebSocketMessage {
  type: string;
  data: unknown;
  timestamp: string;
}

class SKZApiClient {
  private baseUrl: string;
  private wsConnections: Map<string, WebSocket> = new Map();

  constructor(baseUrl: string = '') {
    this.baseUrl = baseUrl;
  }

  private async request<T>(
    endpoint: string,
    options?: RequestInit
  ): Promise<AgentResponse<T>> {
    try {
      const response = await fetch(`${this.baseUrl}${endpoint}`, {
        headers: {
          'Content-Type': 'application/json',
          ...options?.headers,
        },
        ...options,
      });

      const data = await response.json();

      return {
        success: response.ok,
        data: response.ok ? data : undefined,
        error: response.ok ? undefined : data.error || 'Request failed',
        timestamp: new Date().toISOString(),
      };
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Network error',
        timestamp: new Date().toISOString(),
      };
    }
  }

  // Agent status endpoints
  async getAgentStatus(agentName: string): Promise<AgentResponse<AgentStatus>> {
    return this.request<AgentStatus>(`/api/agents/${agentName}/status`);
  }

  async getAllAgentsStatus(): Promise<AgentResponse<AgentStatus[]>> {
    return this.request<AgentStatus[]>('/api/agents/status');
  }

  // Research Discovery Agent
  async getResearchData(): Promise<AgentResponse<ResearchData>> {
    return this.request<ResearchData>('/api/agents/research-discovery/data');
  }

  async searchPatents(
    query: string
  ): Promise<AgentResponse<PatentSearchResult[]>> {
    return this.request('/api/agents/research-discovery/search-patents', {
      method: 'POST',
      body: JSON.stringify({ query }),
    });
  }

  // Submission Assistant Agent
  async getSubmissionData(): Promise<AgentResponse<SubmissionData>> {
    return this.request<SubmissionData>(
      '/api/agents/submission-assistant/data'
    );
  }

  async assessSubmission(
    submissionId: string
  ): Promise<AgentResponse<AssessmentResult>> {
    return this.request(
      `/api/agents/submission-assistant/assess/${submissionId}`,
      {
        method: 'POST',
      }
    );
  }

  // Editorial Orchestration Agent
  async getEditorialData(): Promise<AgentResponse<EditorialData>> {
    return this.request<EditorialData>(
      '/api/agents/editorial-orchestration/data'
    );
  }

  async resolveConflict(
    workflowId: string,
    decision: ConflictDecision
  ): Promise<AgentResponse<ConflictResolution>> {
    return this.request(
      `/api/agents/editorial-orchestration/resolve-conflict/${workflowId}`,
      {
        method: 'POST',
        body: JSON.stringify(decision),
      }
    );
  }

  // Review Coordination Agent
  async getReviewData(): Promise<AgentResponse<ReviewData>> {
    return this.request<ReviewData>('/api/agents/review-coordination/data');
  }

  async assignReviewer(
    submissionId: string,
    reviewerId: string
  ): Promise<AgentResponse<ReviewAssignment>> {
    return this.request('/api/agents/review-coordination/assign', {
      method: 'POST',
      body: JSON.stringify({ submissionId, reviewerId }),
    });
  }

  // Content Quality Agent
  async getQualityData(): Promise<AgentResponse<QualityData>> {
    return this.request<QualityData>('/api/agents/content-quality/data');
  }

  async validateContent(
    submissionId: string
  ): Promise<AgentResponse<ValidationResult>> {
    return this.request(
      `/api/agents/content-quality/validate/${submissionId}`,
      {
        method: 'POST',
      }
    );
  }

  // Publishing Production Agent
  async getPublishingData(): Promise<AgentResponse<PublishingData>> {
    return this.request<PublishingData>(
      '/api/agents/publishing-production/data'
    );
  }

  async publishContent(
    submissionId: string,
    channels: string[]
  ): Promise<AgentResponse<PublicationResult>> {
    return this.request('/api/agents/publishing-production/publish', {
      method: 'POST',
      body: JSON.stringify({ submissionId, channels }),
    });
  }

  // Analytics & Monitoring Agent
  async getAnalyticsData(): Promise<AgentResponse<AnalyticsData>> {
    return this.request<AnalyticsData>('/api/agents/analytics-monitoring/data');
  }

  // WebSocket connections for real-time updates
  connectWebSocket(
    agentName: string,
    onMessage: (data: WebSocketMessage) => void
  ): void {
    const wsUrl = `${this.baseUrl.replace('http', 'ws')}/ws/agents/${agentName}`;
    const ws = new WebSocket(wsUrl);

    ws.onmessage = (event) => {
      try {
        const data = JSON.parse(event.data);
        onMessage(data);
      } catch (error) {
        console.error('Failed to parse WebSocket message:', error);
      }
    };

    ws.onopen = () => {
      console.log(`WebSocket connected for agent: ${agentName}`);
    };

    ws.onclose = () => {
      console.log(`WebSocket disconnected for agent: ${agentName}`);
      // Attempt to reconnect after a delay
      setTimeout(() => this.connectWebSocket(agentName, onMessage), 5000);
    };

    ws.onerror = (error) => {
      console.error(`WebSocket error for agent ${agentName}:`, error);
    };

    this.wsConnections.set(agentName, ws);
  }

  disconnectWebSocket(agentName: string): void {
    const ws = this.wsConnections.get(agentName);
    if (ws) {
      ws.close();
      this.wsConnections.delete(agentName);
    }
  }

  disconnectAllWebSockets(): void {
    this.wsConnections.forEach((ws) => {
      ws.close();
    });
    this.wsConnections.clear();
  }
}

// Export singleton instance
export const skzApi = new SKZApiClient();

// Export mock data for development/testing
export const mockData = {
  agentStatus: [
    {
      id: 'research-discovery',
      name: 'Research Discovery Agent',
      status: 'active' as const,
      lastUpdate: new Date().toISOString(),
    },
    {
      id: 'submission-assistant',
      name: 'Submission Assistant Agent',
      status: 'active' as const,
      lastUpdate: new Date().toISOString(),
    },
    {
      id: 'editorial-orchestration',
      name: 'Editorial Orchestration Agent',
      status: 'idle' as const,
      lastUpdate: new Date().toISOString(),
    },
    {
      id: 'review-coordination',
      name: 'Review Coordination Agent',
      status: 'active' as const,
      lastUpdate: new Date().toISOString(),
    },
    {
      id: 'content-quality',
      name: 'Content Quality Agent',
      status: 'active' as const,
      lastUpdate: new Date().toISOString(),
    },
    {
      id: 'publishing-production',
      name: 'Publishing Production Agent',
      status: 'idle' as const,
      lastUpdate: new Date().toISOString(),
    },
    {
      id: 'analytics-monitoring',
      name: 'Analytics & Monitoring Agent',
      status: 'active' as const,
      lastUpdate: new Date().toISOString(),
    },
  ],
  researchData: {
    trends: [
      { keyword: 'retinol', count: 142, trend: 'up' as const },
      { keyword: 'peptides', count: 98, trend: 'up' as const },
      { keyword: 'niacinamide', count: 87, trend: 'stable' as const },
    ],
    patents: [
      {
        id: 'US123456',
        title: 'Novel Vitamin C Delivery System',
        relevance: 0.95,
      },
      { id: 'EP789012', title: 'Anti-aging Peptide Complex', relevance: 0.87 },
    ],
    inciData: [
      { name: 'Aqua', category: 'Solvent', safetyRating: 10 },
      { name: 'Retinol', category: 'Active', safetyRating: 7 },
    ],
  },
};
