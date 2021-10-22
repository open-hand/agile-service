import useCategoryCodes, { ICategoryCode } from './useCategoryCodes';

const useHasDevops = (propCodes?: ICategoryCode[]) => {
  const codes = useCategoryCodes();
  return (propCodes || codes).includes('N_DEVOPS');
};

export default useHasDevops;
