import useCategoryCodes from './useCategoryCodes';

const useIsProgramProject = () => {
  const codes = useCategoryCodes();
  return { isProgramProject: codes.includes('N_PROGRAM_PROJECT') || (codes.includes('N_PROGRAM') && codes.includes('N_AGILE')) };
};

export default useIsProgramProject;
