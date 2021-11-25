import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  'import.issue': 'Import Issues',
  'export.issue': 'Export Issues',
  route: 'Issues',

} as const;
const exportIssue = localeAppendPrefixObjectKey({ intlPrefix: 'issue' as const, intlObject: locale });
type ILocaleIssueType = {
  ['agile.issue']: Array<keyof typeof locale>[number]
}
export { exportIssue };
export type { ILocaleIssueType };
