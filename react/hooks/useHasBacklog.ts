import useCategoryCodes, { ICategoryCode } from './useCategoryCodes';

const useHasBacklog = (propCodes?: ICategoryCode[]) => {
  const codes = useCategoryCodes();
  return (propCodes || codes).includes('N_REQUIREMENT');
};

export default useHasBacklog;
