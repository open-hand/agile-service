import useBurnDownReport, { BurnDownConfig } from '../burn-down/useBurnDownReport';

export type SprintConfig = Pick<BurnDownConfig, 'sprintId' | 'restDayShow' | 'projectId'>

function useSprintReport(config?: SprintConfig) {
  return useBurnDownReport({
    type: 'issueCount',
    ...config,
  });
}

export default useSprintReport;
