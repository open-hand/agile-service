import { stores } from '@choerodon/boot';

const { AppState } = stores;

const useCategoryCodes = () => (AppState.menuType.categories ? AppState.menuType.categories.map((c: any) => c.code) : []);
const useHasDevops = () => {
  const codes = useCategoryCodes();
  return codes.includes('N_DEVOPS');
};

export default useHasDevops;
