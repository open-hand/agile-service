import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  'collapse.complete.epic': '收起已完成的史诗列',
  'hide.empty.story': '隐藏无故事的列',
  'no.swimlane': '无泳道',
  'no.plan.list': '未规划列表',
  'version.swimlane': '版本泳道',
  'sprint.swimlane': '冲刺泳道',
  'empty.data.description': '用户故事地图是以史诗为基础，根据版本控制进行管理规划',
} as const;
const exportStoryMap = localeAppendPrefixObjectKey({ intlPrefix: 'storyMap' as const, intlObject: locale });
type ILocaleStoryMapType = {
  ['agile.storyMap']: Array<keyof typeof locale>[number]
}
export { exportStoryMap };
export type { ILocaleStoryMapType };
