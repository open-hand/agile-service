import useCategoryCodes, { IHookCategoryCodesConfig } from './useCategoryCodes';

const useIsAgile = (config?:IHookCategoryCodesConfig) => {
  const codes = useCategoryCodes(config);
  return { isAgile: codes.includes('N_AGILE') };
};

export default useIsAgile;
