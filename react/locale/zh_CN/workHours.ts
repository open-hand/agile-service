import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  unsaturate: '未饱和',
  saturate: '饱和',
  'over.saturate': '过度饱和',
  'work.log': '工时日志',

} as const;
const exportWorkHours = localeAppendPrefixObjectKey({ intlPrefix: 'workHours' as const, intlObject: locale });
type ILocaleWorkHoursType = {
  ['agile.workHours']: Array<keyof typeof locale>[number]
}
export { exportWorkHours };
export type { ILocaleWorkHoursType };
