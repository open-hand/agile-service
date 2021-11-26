import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  route: 'Issue types',
  reference: 'Reference issue type',
  name: 'Scheme Name',
  add: 'Add Issue type',
  'norm.type': 'Item Types',
  state: 'States',
  description: 'Description',
} as const;
const exportIssueType = localeAppendPrefixObjectKey({ intlPrefix: 'issueType' as const, intlObject: locale });
type ILocaleIssueTypeType = {
  ['agile.issueType']: Array<keyof typeof locale>[number]
}
export { exportIssueType };
export type { ILocaleIssueTypeType };
