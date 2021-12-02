import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  route: '待办事项',
  'create.sprint': '创建冲刺',
  'current.create.sprint': '当前PI下创建冲刺',
  'show.init.sprint': '显示未开始冲刺',
  'assignee.workload': '查看经办人工作量',
  'visible.issue': '{issue}个工作项可见',
  'start.sprint': '开启冲刺',
  'global.sprint': '冲刺目标',
  'delete.sprint': '删除冲刺',
  'no.start': '未开始',
  'empty.sprint.data.title': '计划您的SPRINT',
  'empty.sprint.data.description': '这是一个Sprint。将工作项拖拽至此来计划一个Sprint。',
  'empty.backlog.data.title': '当前项目暂无待办事项',
  'empty.backlog.data.description': '您可以在此创建并评估工作项，可通过上下拖动来规划工作项的排列顺序',

} as const;
const exportBacklog = localeAppendPrefixObjectKey({ intlPrefix: 'backlog' as const, intlObject: locale });
type ILocaleBacklogType = {
  ['agile.backlog']: Array<keyof typeof locale>[number]
}
export { exportBacklog };
export type { ILocaleBacklogType };
