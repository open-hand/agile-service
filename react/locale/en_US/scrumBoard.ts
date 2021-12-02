import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  'column.config': 'Column Management',
  'column.setting': 'Board Settings',
  swimlane: 'Swimlanes',
  'working.day': 'Working Days',
  'board.name': 'Board Name',
  'delete.board': 'Delete Board {name}',
  'board.swimlane.in': 'Base Swimlanes on',
  'add.column': 'Add Column',
  'create.board': 'Create Board',
  'remain.day': '{day} days remaining',
  filter: 'Filter',
  'create.filter': 'Create Filter',
  'choose.quick.filter': 'Choose Quick Filter',
  'create.more.filter': 'Create More Filter',
  'hidden.sub_task.completed.in.history.sprint': 'Hidden sub_task completed in history sprint',
  'hide.sub_task.completed.in.history.sprint': 'Hide sub_task completed in history sprint',
  'empty.description.prefix.sprint': 'No active sprints',
  'empty.description.prefix.plan': 'The Sprint is empty',
  'empty.description.prefix.filter': 'Under the filter,the data is empty',
  'empty.description': '{prefix},<s1>start sprint</s1><s2>plan issues to current sprint </s2> in the {button}',
} as const;
const exportScrumBoard = localeAppendPrefixObjectKey({ intlPrefix: 'scrumBoard' as const, intlObject: locale });
type ILocaleScrumBoardType = {
  ['agile.scrumBoard']: Array<keyof typeof locale>[number]
}
export { exportScrumBoard };
export type { ILocaleScrumBoardType };
