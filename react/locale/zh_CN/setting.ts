import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  'create.component': '创建模块',
  component: '模块',
  responsible: '负责人',
  'component.description': '模块描述',
  filter: '快速筛选',
  'create.filter': '创建快速筛选',
  'filter.column': '筛选器',
  'create.issue.link': '创建工作项链接',
  'edit.issue.link': '修改工作项链接',
  'issue_link.other': '其他链接',
  'issue_link.name': '名称',
  'issue_link.delete.only': '删除链接',
  'issue_link.outWard': '链出描述',
  'issue_link.inWard': '链入描述',
  'issue_link.checkName.repeat': '工作项链接名称重复',
  'issue_link.delete.link.other': '删除链接，相关工作项关联到其他链接',
} as const;
const exportSetting = localeAppendPrefixObjectKey({ intlPrefix: 'setting' as const, intlObject: locale });
type ILocaleSettingType = {
  ['agile.setting']: Array<keyof typeof locale>[number]
}
export { exportSetting };
export type { ILocaleSettingType };
