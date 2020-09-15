import useBurnDownReport from '../burn-down/useBurnDownReport';

function useSprintReport() {
  return useBurnDownReport({
    type: 'issueCount',
  });
}

export default useSprintReport;
