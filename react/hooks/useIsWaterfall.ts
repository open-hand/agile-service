import useCategoryCodes, { IHookCategoryCodesConfig } from './useCategoryCodes';

const useIsWaterfall = (config?:IHookCategoryCodesConfig) => {
  const codes = useCategoryCodes(config);
  const isWaterfall = codes.includes('N_WATERFALL');
  const isWaterfallAgile = codes.includes('N_WATERFALL_AGILE');
  return { isWaterfall, isWaterfallAgile };
};

export default useIsWaterfall;
