import { HashRouter, Outlet, Route, Routes } from 'react-router';
import Header from './components/Header';
import Sidebar from './components/Sidebar';
import { AppContextProvider, useAppContext } from './utils/app.context';
import ChatScreen from './components/ChatScreen';
import SettingDialog from './components/SettingDialog';
import { Toaster } from 'react-hot-toast';
import { ModalProvider } from './components/ModalProvider';
import SKZDashboard from './components/dashboards/SKZDashboard';
import ResearchDiscoveryDashboard from './components/dashboards/ResearchDiscoveryDashboard';
import SubmissionAssistantDashboard from './components/dashboards/SubmissionAssistantDashboard';
import AnalyticsMonitoringDashboard from './components/dashboards/AnalyticsMonitoringDashboard';
import PlaceholderDashboard from './components/dashboards/PlaceholderDashboard';
import {
  CogIcon,
  UserGroupIcon,
  ChartBarIcon,
  PrinterIcon,
} from '@heroicons/react/24/outline';

function App() {
  return (
    <ModalProvider>
      <HashRouter>
        <div className="flex flex-row drawer lg:drawer-open">
          <AppContextProvider>
            <Routes>
              <Route element={<AppLayout />}>
                <Route path="/dashboard" element={<SKZDashboard />} />
                <Route
                  path="/dashboard/research-discovery"
                  element={<ResearchDiscoveryDashboard />}
                />
                <Route
                  path="/dashboard/submission-assistant"
                  element={<SubmissionAssistantDashboard />}
                />
                <Route
                  path="/dashboard/editorial-orchestration"
                  element={
                    <PlaceholderDashboard
                      agentName="Editorial Orchestration Agent"
                      description="Workflow coordination, decision making, and conflict resolution"
                      icon={CogIcon}
                    />
                  }
                />
                <Route
                  path="/dashboard/review-coordination"
                  element={
                    <PlaceholderDashboard
                      agentName="Review Coordination Agent"
                      description="Reviewer matching, workload management, and quality monitoring"
                      icon={UserGroupIcon}
                    />
                  }
                />
                <Route
                  path="/dashboard/content-quality"
                  element={
                    <PlaceholderDashboard
                      agentName="Content Quality Agent"
                      description="Scientific validation, safety assessment, and standards enforcement"
                      icon={ChartBarIcon}
                    />
                  }
                />
                <Route
                  path="/dashboard/publishing-production"
                  element={
                    <PlaceholderDashboard
                      agentName="Publishing Production Agent"
                      description="Content formatting, visual generation, and multi-channel distribution"
                      icon={PrinterIcon}
                    />
                  }
                />
                <Route
                  path="/dashboard/analytics-monitoring"
                  element={<AnalyticsMonitoringDashboard />}
                />
                <Route path="/chat/:convId" element={<ChatScreen />} />
                <Route path="*" element={<ChatScreen />} />
              </Route>
            </Routes>
          </AppContextProvider>
        </div>
      </HashRouter>
    </ModalProvider>
  );
}

function AppLayout() {
  const { showSettings, setShowSettings } = useAppContext();
  return (
    <>
      <Sidebar />
      <main
        className="drawer-content grow flex flex-col h-screen mx-auto px-4 overflow-auto bg-base-100"
        id="main-scroll"
      >
        <Header />
        <Outlet />
      </main>
      {
        <SettingDialog
          show={showSettings}
          onClose={() => setShowSettings(false)}
        />
      }
      <Toaster />
    </>
  );
}

export default App;
