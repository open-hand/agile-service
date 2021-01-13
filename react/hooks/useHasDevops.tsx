import useCategoryCodes from './useCategoryCodes';

const useHasDevops = () => {
  const codes = useCategoryCodes();
  return codes.includes('N_DEVOPS');
};

export default useHasDevops;
