import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  route: 'Version',
  'publish.route': 'Release Versions',
  create: 'Create Version',
  status: 'Status',
  'state.date': 'Start Date',
  'expect.release.date': 'Expected Release Date',
  'actual.release.date': 'Actual Release Date',
  'description.tooltip': 'Description：{text}',
} as const;
const exportVersion = localeAppendPrefixObjectKey({ intlPrefix: 'version' as const, intlObject: locale });
type ILocaleVersionType = {
  ['agile.version']: Array<keyof typeof locale>[number]
}
export { exportVersion };
export type { ILocaleVersionType };
