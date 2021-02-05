import useCategoryCodes from './useCategoryCodes';

const useHasTest = () => {
  const codes = useCategoryCodes();
  return codes.includes('N_TEST');
};

export default useHasTest;
