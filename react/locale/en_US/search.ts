import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  startTime: 'Start Time',
  endTime: 'End Time',
  myFilter: 'My Filter',
  search: 'Search',
  'clear.filter': 'Clear',
  'common.option': 'Common filters',
  'only.me.issue': 'Only my issues',
  'my.star': 'Started issues',
  'my.handle': 'Assigned issues',
} as const;
const exportSearch = localeAppendPrefixObjectKey({ intlPrefix: 'search' as const, intlObject: locale });
type ILocaleSearchType = {
  ['agile.search']: Array<keyof typeof locale>[number]
}
export { exportSearch };
export type { ILocaleSearchType };
