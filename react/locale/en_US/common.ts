import { localeAppendPrefixObjectKey } from '@/utils/locale';

const localeCommon = {
  field: 'Field',
  priority: 'Priority',
  description: 'Description',
  default: 'Default',
  'create.issue': 'Create Issue',
  'collapse.all': 'Collapse all',
  'expand.all': 'Expand all',
  assignee: 'Assignees',
  epic: 'Epics',
  story: 'Stories',
  'save.filter': 'Save Filter',
  reset: 'Reset',
  none: 'None',
  'add.status': 'Add Status',
  'add.filter': 'Add',
  'personal.filter': 'Personal Filter',
  'complete.sprint': 'Complete Sprint',
  version: 'Versions',
  feature: 'Features',
  active: 'Active',
  complete: 'Complete',
  'tree.view': 'Tree View',
  'list.view': 'List View',
  summary: 'Summary',
  'fix.version': 'Fix Versions',
  'influence.version': 'Affects Versions',
  status: 'Status',
  component: 'Components',
  label: 'Labels',
  sprint: 'Sprint',
  environment: 'Environment',
  reporter: 'Reporter',
  'origin.estimate': 'Original Estimate',
  'remain.estimate': 'Remaining Estimate',
  'stroy.point': 'Story Points',
  key: 'Key',
  'column.config': 'Column Configuration',
  creator: 'Creator',
  'update.user': 'Updated by',
  title: 'Title',
  issue: 'Issue',
  name: 'Name',
  project: 'Project',
  system: 'System',
  organization: 'Organization',
  issueType: 'IssueType',

} as const;
const exportCommon = localeAppendPrefixObjectKey({ intlPrefix: 'common' as const, intlObject: localeCommon });
type ILocaleCommonType = {
  ['agile.common']: Array<keyof typeof localeCommon>[number]
}
export { exportCommon };
export type { ILocaleCommonType };
