import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  route: '页面',
  'field.name': '字段名称',
  'field.import': '导入字段',
  'field.create': '创建字段',
  'field.edit': '修改字段',
  'field.range': '字段范围',
  'field.source': '字段来源',
  'field.type': '字段类型',
  'field.required': '必填项',
  'field.can.edit': '加入到编辑页',
  'field.can.create': '加入到创建页',
  'field.operate': '操作',
  'field.default': '默认值',
  'field.context': '工作项类型',
  default: '默认值',
  template: '页面模板',
  'add.exist': '添加已有字段',
  'template.description.format': '描述信息格式',
  'template.role': '角色权限说明',
  'template.cascade': '级联说明',
  'template.name': '字段名称',
  config: '页面配置',
  'config.project.require': '必填',
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
