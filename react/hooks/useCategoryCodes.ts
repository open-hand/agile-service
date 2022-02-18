import { stores } from '@choerodon/boot';
import { useObserver } from 'mobx-react-lite';

const { AppState } = stores;
export type ICategoryCode =
  // 需求管理
  'N_REQUIREMENT' |
  // devops
  'N_DEVOPS' |
  // 测试管理
  'N_TEST' |
  // 敏捷项目
  'N_AGILE' |
  // 子项目
  'N_PROGRAM_PROJECT' |
  // 瀑布项目
  'N_WATERFALL' |
  // 项目群
  'N_PROGRAM' |
  // 运维项目
  'N_OPERATIONS' |
  // 大瀑布小敏捷
  'N_WATERFALL_AGILE'

const useCategoryCodes = (): ICategoryCode[] => (
  useObserver(() => (AppState.menuType.categories
    ? AppState.menuType.categories.map((c: any) => c.code)
    : [])));

export default useCategoryCodes;
