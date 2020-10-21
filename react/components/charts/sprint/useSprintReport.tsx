import useBurnDownReport, { BurnDownConfig } from '../burn-down/useBurnDownReport';

export type SprintConfig = Pick<BurnDownConfig, 'sprintId' | 'restDayShow' | 'projectId' | 'useCurrentSprint'>

function useSprintReport(config?: SprintConfig, onFinish?: Function) {
  return useBurnDownReport({
    type: 'issueCount',
    ...config,
  }, onFinish);
}

export default useSprintReport;
