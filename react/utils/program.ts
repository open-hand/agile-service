import { stores } from '@choerodon/boot';

const { AppState } = stores;
export function isInProgram() {
  const categories = AppState.menuType.categories
    ? AppState.menuType.categories.map((c: any) => c.code)
    : [];
  return categories.includes('N_PROGRAM_PROJECT');
}
