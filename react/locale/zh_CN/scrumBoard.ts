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
  filter: '筛选',
  'create.filter': '创建筛选',
  'choose.quick.filter': '选择快速筛选',
  'create.more.filter': '创建更多快速筛选',
  'hidden.sub_task.completed.in.history.sprint': '已隐藏历史迭代中已完成的子任务',
  'hide.sub_task.completed.in.history.sprint': '隐藏在历史迭代已完成的子任务',
} as const;
const exportScrumBoard = localeAppendPrefixObjectKey({ intlPrefix: 'scrumBoard' as const, intlObject: locale });
type ILocaleScrumBoardType = {
  ['agile.scrumBoard']: Array<keyof typeof locale>[number]
}
export { exportScrumBoard };
export type { ILocaleScrumBoardType };
