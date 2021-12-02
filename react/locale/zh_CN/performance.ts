import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  route: '绩效',
  'progress.efficiency': '进度与效率',
  'story.points.distribution': '故事点分布',
  'story.completed': '故事完成情况',
  quality: '质量分析',
  trend: '趋势分析',

} as const;
const exportPerformance = localeAppendPrefixObjectKey({ intlPrefix: 'performance' as const, intlObject: locale });
type ILocalePerformanceType = {
  ['agile.performance']: Array<keyof typeof locale>[number]
}
export { exportPerformance };
export type { ILocalePerformanceType };
