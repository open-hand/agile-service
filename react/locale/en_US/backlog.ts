import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  route: 'Backlog',
  'create.sprint': 'Create Sprint',
  'current.create.sprint': 'Create Sprint in PI',
  'show.init.sprint': 'Show Unstarted Sprint',
  'assignee.workload': 'Workload By Assignee',
  'visible.issue': '{issue} issues',
  'start.sprint': 'Start Sprint',
  'global.sprint': 'Sprint Goal',
  'delete.sprint': 'Delete Sprint',
  'no.start': 'Unstarted',
  'empty.sprint.data.title': 'Plan your sprint',
  'empty.sprint.data.description': 'Sprint：Here is a sprint.Drag issues here to plan the work for this sprint',
  'empty.backlog.data.title': 'The Backlog is empty',
  'empty.backlog.data.description': 'You can create and evaluate issues here. You can drag up or drag down to plan the order of issues',

} as const;
const exportBacklog = localeAppendPrefixObjectKey({ intlPrefix: 'backlog' as const, intlObject: locale });
type ILocaleBacklogType = {
  ['agile.backlog']: Array<keyof typeof locale>[number]
}
export { exportBacklog };
export type { ILocaleBacklogType };
