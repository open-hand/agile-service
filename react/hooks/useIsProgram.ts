import useCategoryCodes from './useCategoryCodes';
import useIsAgile from './useIsAgile';

const useIsProgram = () => {
  const codes = useCategoryCodes();
  const { isAgile } = useIsAgile();
  const isProgram = codes.includes('N_PROGRAM');
  return { isProgram, isAgileProgram: isProgram && isAgile };
};

export default useIsProgram;
