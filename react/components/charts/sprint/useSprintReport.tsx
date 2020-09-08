import useBurnDownReport from '../burn-down/useBurnDownReport';

function useSprintReport() {
  return useBurnDownReport({
    defaultType: 'issueCount',
  });
}

export default useSprintReport;
