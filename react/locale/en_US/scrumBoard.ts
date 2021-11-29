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
} as const;
const exportScrumBoard = localeAppendPrefixObjectKey({ intlPrefix: 'scrumBoard' as const, intlObject: locale });
type ILocaleScrumBoardType = {
  ['agile.scrumBoard']: Array<keyof typeof locale>[number]
}
export { exportScrumBoard };
export type { ILocaleScrumBoardType };
