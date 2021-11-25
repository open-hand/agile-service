import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  'import.issue': '导入工作项',
  'export.issue': '导出工作项',
  route: '所有工作项',

} as const;
const exportIssue = localeAppendPrefixObjectKey({ intlPrefix: 'issue' as const, intlObject: locale });
type ILocaleIssueType = {
  ['agile.issue']: Array<keyof typeof locale>[number]
}
export { exportIssue };
export type { ILocaleIssueType };
