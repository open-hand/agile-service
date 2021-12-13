import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  route: 'Performance',
  'progress.efficiency': 'Schedule & Efficiency',
  'story.points.distribution': 'Story Point Distribution',
  'story.completed': 'Story completion',
  quality: 'Quality Analysis',
  trend: 'Trend Analysis',
  distribution: '{name} Distribution',
} as const;
const exportPerformance = localeAppendPrefixObjectKey({ intlPrefix: 'performance' as const, intlObject: locale });
type ILocalePerformanceType = {
  ['agile.performance']: Array<keyof typeof locale>[number]
}
export { exportPerformance };
export type { ILocalePerformanceType };
