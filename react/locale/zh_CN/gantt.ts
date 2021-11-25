import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  'view.issue': '按任务查看',
  'view.assignee': '按经办人查看',
  'view.sprint': '按冲刺查看',
  'view.epic': '按史诗查看',
  'work.time.count': '工时计数',
  'issue.count': '工作项个数',
  legend: '图例说明',
  day: '日',
  week: '周',
  month: '月',
  today: '返回今日',
  quarter: '季',
  year: '年',
} as const;
const exportGantt = localeAppendPrefixObjectKey({ intlPrefix: 'gantt' as const, intlObject: locale });
type ILocaleGanttType = {
  ['agile.gantt']: Array<keyof typeof locale>[number]
}
export { exportGantt };
export type { ILocaleGanttType };
