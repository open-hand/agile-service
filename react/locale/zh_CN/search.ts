import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  startTime: '开始时间',
  endTime: '结束时间',
  myFilter: '我的筛选',
  search: '请输入搜索内容',
} as const;
const exportSearch = localeAppendPrefixObjectKey({ intlPrefix: 'search' as const, intlObject: locale });
type ILocaleSearchType = {
  ['agile.search']: Array<keyof typeof locale>[number]
}
export { exportSearch };
export type { ILocaleSearchType };
