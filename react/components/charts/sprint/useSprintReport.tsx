import useBurnDownReport, { BurnDownConfig } from '../burn-down/useBurnDownReport';

export type SprintConfig = Omit<BurnDownConfig, 'type'>

function useSprintReport(config?: SprintConfig) {
  return useBurnDownReport({
    type: 'issueCount',
    ...config,
  });
}

export default useSprintReport;
