import useCategoryCodes from './useCategoryCodes';

const useIsProgram = () => {
  const codes = useCategoryCodes();
  return { isProgram: codes.includes('N_PROGRAM') };
};

export default useIsProgram;
