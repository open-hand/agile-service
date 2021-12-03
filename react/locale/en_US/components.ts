import { localeAppendPrefixObjectKey } from '@/utils/locale';

const localeField = {
  quickFilter: 'Quick filter',
  summary: 'Summary',
  issueType: 'IssueType',
  status: 'Status',
  assignee: 'Assignee',
  reporter: 'Reporter',
  sprint: 'Sprint',
  component: 'Components',
  label: 'Labels',
  priority: 'Priority',
  version: 'Version',
  fixVersion: 'Fix Versions',
  influenceVersion: 'Affects Versions',
  epic: 'Epics',
  feature: 'Features',
  createDate: 'Creation Time',
  updateDate: 'Update Time',
  estimatedStartTime: ' Estimated Start Time',
  estimatedEndTime: 'Estimated End Time',
  actualStartTime: 'Actual Start Time',
  actualEndTime: 'Actual End Time',
  mainResponsible: 'Main Responsible',
  environment: 'Environment',
  creator: 'Creator',
  updater: 'Updater',
  participant: 'Participant',
  storyPointsNull: '故事点为空',
  remainingTimeNull: '剩余预估时间为空',
  tag: 'Tag',
  storyPoint: 'Story Points',
  remainingTime: 'Remaining Estimate',
  estimateTime: 'Original Estimate',
} as const;
const exportSystemField = localeAppendPrefixObjectKey({ intlPrefix: 'systemField' as const, intlObject: localeField });
type ILocaleSystemFieldType = {
  ['agile.systemField']: Array<keyof typeof localeField>[number]
}
export { exportSystemField };
export type { ILocaleSystemFieldType };
