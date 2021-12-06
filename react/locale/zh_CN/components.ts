import { localeAppendPrefixObjectKey } from '@/utils/locale';

const localeField = {
  quickFilter: '快速筛选',
  summary: '概要',
  issueType: '工作项类型',
  status: '状态',
  assignee: '经办人',
  reporter: '报告人',
  sprint: '冲刺',
  component: '模块',
  label: '标签',
  priority: '优先级',
  version: '版本',
  fixVersion: '修复的版本',
  influenceVersion: '影响的版本',
  epic: '史诗',
  feature: '特性',
  createDate: '创建时间',
  updateDate: '更新时间',
  estimatedStartTime: '预计开始时间',
  estimatedEndTime: '预计结束时间',
  actualStartTime: '实际开始时间',
  actualEndTime: '实际结束时间',
  mainResponsible: '主要负责人',
  environment: '环境',
  creator: '创建人',
  updater: '更新人',
  participant: '参与人',
  storyPointsNull: '故事点为空',
  remainingTimeNull: '剩余预估时间为空',
  tag: 'Tag',
  storyPoint: '故事点',
  remainingTime: '剩余预估时间',
  estimateTime: '原始预估时间',
} as const;

const localeColumnField = {
  lastUpdateDate: '最近更新时间',
  spentWorkTime: '已耗费时间',
  allEstimateTime: '当前预估时间',
  deviationRate: '偏差率',
  cumulativeWorkTime: '历史累计工时',
  workTime: '工时',
  epicSelfName: '史诗名称',
} as const;
const exportSystemField = localeAppendPrefixObjectKey({ intlPrefix: 'systemField' as const, intlObject: localeField });
const exportColumnField = localeAppendPrefixObjectKey({ intlPrefix: 'columnField' as const, intlObject: localeColumnField });
type ILocaleSystemFieldType = {
  ['agile.columnField']: Array<keyof typeof localeColumnField>[number]
  ['agile.systemField']: Array<keyof typeof localeField>[number]
}
export { exportSystemField, exportColumnField };
export type { ILocaleSystemFieldType };
