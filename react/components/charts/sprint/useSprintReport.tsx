import useBurnDownReport, { BurnDownConfig } from '../burn-down/useBurnDownReport';
import useGetChartSearchDataSet from '../useGetChartSearchDataSet';

export type SprintConfig = Pick<BurnDownConfig, 'sprintId' | 'restDayShow' | 'projectId' | 'useCurrentSprint' | 'openValidate'>

function useSprintReport(config?: SprintConfig, onFinish?: Function) {
  const [searchProps, props] = useBurnDownReport({
    type: 'issueCount',
    ...config,
    openValidate: false,
  }, onFinish);
  const searchDataSet = useGetChartSearchDataSet({
    enabled: config?.openValidate,
    fields: [
      { name: 'sprint', label: '迭代冲刺', required: true },
    ],
    valueChangeDataSetValue: {
      sprint: searchProps.useCurrentSprint ? 'current' : searchProps.sprintId,
    },
  });
  return [{ ...searchProps, searchDataSet }, props] as const;
}

export default useSprintReport;
