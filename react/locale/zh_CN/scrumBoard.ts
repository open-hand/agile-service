import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  'column.config': '列配置',
  'column.setting': '配置看板',
  swimlane: '泳道',
  'working.day': '工作日历',
  'board.name': '看板名称',
  'delete.board': '删除看板{name}',
  'board.swimlane.in': '基础泳道在',
  'add.column': '添加列',
  'create.board': '创建看板',
  'remain.day': '{day} days 剩余',

} as const;
const exportScrumBoard = localeAppendPrefixObjectKey({ intlPrefix: 'scrumBoard' as const, intlObject: locale });
type ILocaleScrumBoardType = {
  ['agile.scrumBoard']: Array<keyof typeof locale>[number]
}
export { exportScrumBoard };
export type { ILocaleScrumBoardType };
