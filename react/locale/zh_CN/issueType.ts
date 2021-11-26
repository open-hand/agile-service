import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  route: '问题类型',
  reference: '引用工作项类型',
  name: '名称',
  add: '添加工作项类型',
  'norm.type': '标准工作项类型',
  source: '来源',
  state: '状态',
  description: '工作项类型描述',
} as const;
const exportIssueType = localeAppendPrefixObjectKey({ intlPrefix: 'issueType' as const, intlObject: locale });
type ILocaleIssueTypeType = {
  ['agile.issueType']: Array<keyof typeof locale>[number]
}
export { exportIssueType };
export type { ILocaleIssueTypeType };
