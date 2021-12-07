import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  'create.component': 'Create',
  component: 'Module',
  responsible: 'Owner',
  'component.description': 'Description',
  filter: 'Filters',
  'create.filter': 'Create',
  'filter.column': 'Filter',
  'create.issue.link': 'Create',
  'edit.issue.link': 'Edit',
  'issue_link.other': '其他链接',
  'issue_link.name': 'Name',
  'issue_link.delete.only': '删除链接',
  'issue_link.outWard': 'Outward Link Description',
  'issue_link.inWard': 'Inward Link Description',
  'issue_link.checkName.repeat': '工作项链接名称重复',
  'issue_link.delete.link.other': '删除链接，相关工作项关联到其他链接',
  'default.assignee': 'Assignee',
  sequence: 'Sequence',
} as const;
const exportSetting = localeAppendPrefixObjectKey({ intlPrefix: 'setting' as const, intlObject: locale });
type ILocaleSettingType = {
  ['agile.setting']: Array<keyof typeof locale>[number]
}
export { exportSetting };
export type { ILocaleSettingType };
