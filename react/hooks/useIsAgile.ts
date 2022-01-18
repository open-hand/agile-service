import useCategoryCodes from './useCategoryCodes';

const useIsAgile = () => {
  const codes = useCategoryCodes();
  return { isAgile: codes.includes('N_AGILE') };
};

export default useIsAgile;
