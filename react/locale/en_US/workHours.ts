import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  unsaturate: 'Unsaturated',
  saturate: 'Saturated',
  'over.saturate': 'Oversaturated',
  'work.log': 'Work Log',

} as const;
const exportWorkHours = localeAppendPrefixObjectKey({ intlPrefix: 'workHours' as const, intlObject: locale });
type ILocaleWorkHoursType = {
  ['agile.workHours']: Array<keyof typeof locale>[number]
}
export { exportWorkHours };
export type { ILocaleWorkHoursType };
