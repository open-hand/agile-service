import useCategoryCodes, { IHookCategoryCodesConfig } from './useCategoryCodes';
import useIsAgile from './useIsAgile';

const useIsProgram = (config?:IHookCategoryCodesConfig) => {
  const codes = useCategoryCodes(config);
  const { isAgile } = useIsAgile(config);
  const isProgram = codes.includes('N_PROGRAM');
  return { isProgram, isAgileProgram: isProgram && isAgile };
};

export default useIsProgram;
