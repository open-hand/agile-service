import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  create: 'Create Report',
  receiver: 'Receiver',
} as const;
const exportProjectReport = localeAppendPrefixObjectKey({ intlPrefix: 'projectReport' as const, intlObject: locale });
type ILocaleProjectReportType = {
  ['agile.projectReport']: Array<keyof typeof locale>[number]
}
export { exportProjectReport };
export type { ILocaleProjectReportType };
