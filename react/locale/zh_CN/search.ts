import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  startTime: '开始时间',
  endTime: '结束时间',
  myFilter: '我的筛选',
  search: '请输入搜索内容',
  'clear.filter': '清除筛选项',
  'common.option': '常用选项',
  'only.me.issue': '仅我的工作项',
  'my.star': '我关注的',
  'my.handle': '我经手的',
} as const;
const exportSearch = localeAppendPrefixObjectKey({ intlPrefix: 'search' as const, intlObject: locale });
type ILocaleSearchType = {
  ['agile.search']: Array<keyof typeof locale>[number]
}
export { exportSearch };
export type { ILocaleSearchType };
