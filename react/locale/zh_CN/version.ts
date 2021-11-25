import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  create: '创建规划版本',
  status: '版本状态',
  'state.date': '开始日期',
  'expect.release.date': '预计发布日期',
  'actual.release.date': '实际发布日期',
  'description.tooltip': '描述：{text}',
} as const;
const exportVersion = localeAppendPrefixObjectKey({ intlPrefix: 'version' as const, intlObject: locale });
type ILocaleVersionType = {
  ['agile.version']: Array<keyof typeof locale>[number]
}
export { exportVersion };
export type { ILocaleVersionType };
