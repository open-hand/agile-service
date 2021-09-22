import useCategoryCodes from './useCategoryCodes';

const useIsProgramProject = () => {
  const codes = useCategoryCodes();
  return { isProgramProject: codes.includes('N_PROGRAM_PROJECT') };
};

export default useIsProgramProject;
