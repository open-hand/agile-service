import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  route: 'Backlog',
  'create.sprint': 'Create Sprint',
  'current.create.sprint': 'Create Sprint in PI',
  'show.init.sprint': 'Show Unstarted Sprint',
  'assignee.workload': 'Workload By Assignee',
  'visible.issue': '{issue} visible issues',
  'start.sprint': 'Start Sprint',
  'global.sprint': 'Sprint Goal',
  'delete.sprint': 'Delete Sprint',
  'no.start': 'Unstarted',

} as const;
const exportBacklog = localeAppendPrefixObjectKey({ intlPrefix: 'backlog' as const, intlObject: locale });
type ILocaleBacklogType = {
  ['agile.backlog']: Array<keyof typeof locale>[number]
}
export { exportBacklog };
export type { ILocaleBacklogType };
