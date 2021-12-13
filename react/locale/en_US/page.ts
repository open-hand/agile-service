import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  route: 'Screens',
  'field.name': 'Field',
  'field.import': 'Import',
  'field.create': 'Create',
  'field.edit': 'Edit',
  'field.range': 'Range Of Fields',
  'field.source': 'Source',
  'field.type': 'Type',
  'field.required': 'Mandatory',
  'field.can.edit': 'Add To Edit Page',
  'field.can.create': 'Add To Create Page',
  'field.operate': 'Operation',
  'field.default': 'DefaultValue',
  'field.context': 'Issue Type',
  system: 'System',
  organization: 'Organization',
  default: 'Default',
  template: 'Page template',
  'add.exist': 'Add Existing field',
  'template.description.format': 'Descriptive information format',
  'template.role': 'Role Permission Description',
  'template.cascade': 'Cascade Description',
  config: 'Configuration Page',
  'config.project.require': 'Mandatory',
  'field.delete.title': '删除字段：{name}',
  'field.delete.msg': '确定要删除“{name}”？删除后，将会从所有使用的工作项中删除此字段，字段数据将一并清空。和此字段相关的级联配置、权限配置也将移除。',
  'defaultValue.sync': '同步默认值',
  'field.required.msg': '必填字段请设置默认值！',
} as const;
const exportPage = localeAppendPrefixObjectKey({ intlPrefix: 'page' as const, intlObject: locale });
type ILocalePageType = {
  ['agile.page']: Array<keyof typeof locale>[number]
}
export { exportPage };
export type { ILocalePageType };
