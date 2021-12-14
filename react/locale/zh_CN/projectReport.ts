import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  create: '创建报告',
  receiver: '收件人',
  noReport: '当前项目下无项目报告，请',
} as const;
const exportProjectReport = localeAppendPrefixObjectKey({ intlPrefix: 'projectReport' as const, intlObject: locale });
type ILocaleProjectReportType = {
  ['agile.projectReport']: Array<keyof typeof locale>[number]
}
export { exportProjectReport };
export type { ILocaleProjectReportType };
