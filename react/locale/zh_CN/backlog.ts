import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  route: '代办事项',
  'create.sprint': '创建冲刺',
  'current.create.sprint': '当前PI下创建冲刺',
  'show.init.sprint': '显示未开始冲刺',
  'assignee.workload': '查看经办人工作量',
  'visible.issue': '{issue}个工作项可见',
  'start.sprint': '开启冲刺',
  'global.sprint': '冲刺目标',
  'delete.sprint': '删除冲刺',
  'no.start': '未开始',

} as const;
const exportBacklog = localeAppendPrefixObjectKey({ intlPrefix: 'backlog' as const, intlObject: locale });
type ILocaleBacklogType = {
  ['agile.backlog']: Array<keyof typeof locale>[number]
}
export { exportBacklog };
export type { ILocaleBacklogType };
