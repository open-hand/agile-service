import useCategoryCodes from './useCategoryCodes';

const useHasBacklog = () => {
  const codes = useCategoryCodes();
  return codes.includes('N_REQUIREMENT');
};

export default useHasBacklog;
