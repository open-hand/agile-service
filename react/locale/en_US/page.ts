import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  route: 'Screens',
  'field.name': 'Field',
  'field.import': 'Import',
  'field.create': 'Create',
  'field.range': 'Range Of Fields',
  'field.source': 'Source',
  'field.type': 'Type',
  'field.required': 'Mandatory',
  'filed.can.edit': 'Add To Edit Page',
  'filed.can.create': 'Add To Create Page',
  'filed.operate': 'Operation',
  system: 'System',
  organization: 'Organization',
  default: 'Defaults',
  template: 'Page template',
  'template.description.format': 'Descriptive information format',
  'template.role': 'Role permissions description',
  'template.cascade': 'Cascade description',
  config: 'Configuration Page',
  'config.project.require': 'Mandatory',
  'field.delete.title': '删除字段：{name}',
  'field.delete.msg': '确定要删除“{name}”？删除后，将会从所有使用的工作项中删除此字段，字段数据将一并清空。和此字段相关的级联配置、权限配置也将移除。',
  'defaultValue.sync': '同步默认值',

} as const;
const exportPage = localeAppendPrefixObjectKey({ intlPrefix: 'page' as const, intlObject: locale });
type ILocaleIssueTypeType = {
  ['agile.page']: Array<keyof typeof locale>[number]
}
export { exportPage };
export type { ILocaleIssueTypeType };
