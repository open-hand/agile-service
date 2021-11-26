import { localeAppendPrefixObjectKey } from '@/utils/locale';

const localeCommon = {
  field: '字段',
  priority: '优先级',
  description: '描述',
  default: '默认',
  'create.issue': '创建工作项',
  'collapse.all': '全部收起',
  'expand.all': '全部展开',
  assignee: '经办人',
  epic: '史诗',
  story: '故事',
  'save.filter': '保存筛选',
  reset: '重置',
  none: '无',
  'add.status': '添加状态',
  'add.filter': '添加筛选',
  'personal.filter': '个人筛选',
  'complete.sprint': '完成冲刺',
  version: '版本',
  feature: '特性',
  active: '活跃',
  complete: '已完成',
  'tree.view': '树形视图',
  'list.view': '列表视图',
  summary: '概要',
  'fix.version': '修复的版本',
  'influence.version': '影响的版本',
  status: '状态',
  component: '模块',
  label: '标签',
  sprint: '冲刺',
  environment: '环境',
  reporter: '报告人',
  'origin.estimate': '原始预估时间',
  'remain.estimate': '剩余预估时间',
  'stroy.point': '故事点',
  key: '编号',
  'column.config': '列配置',
  creator: '创建人',
  title: '标题',
} as const;
const exportCommon = localeAppendPrefixObjectKey({ intlPrefix: 'common' as const, intlObject: localeCommon });
type ILocaleCommonType = {
  ['agile.common']: Array<keyof typeof localeCommon>[number]
}
export { exportCommon };
export type { ILocaleCommonType };