import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  'view.issue': 'Issue View',
  'view.assignee': 'Assignee View',
  'view.sprint': 'Sprint View',
  'view.epic': 'Epic View',
  'work.time.count': 'Man-hour count',
  'issue.count': 'Issue Count',
  legend: 'Legend',
  day: 'Days',
  week: 'Weeks',
  month: 'Months',
  today: 'Today',
  quarter: 'Quarters',
  year: 'Years',
  'column.summary': 'Name',
  'column.estimatedStartTime': 'Estimated Start Time',
  'column.estimatedEndTime': 'Estimated End Time',
  'column.actualStartTime': 'Actual Start Time',
  'column.actualEndTime': 'Actual End Time',
  'column.predecessor': 'Pre-dependency',
} as const;
const exportGantt = localeAppendPrefixObjectKey({ intlPrefix: 'gantt' as const, intlObject: locale });
type ILocaleGanttType = {
  ['agile.gantt']: Array<keyof typeof locale>[number]
}
export { exportGantt };
export type { ILocaleGanttType };
