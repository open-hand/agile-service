import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  'custom.chart': '自定义图表',
} as const;
const exportChart = localeAppendPrefixObjectKey({ intlPrefix: 'chart' as const, intlObject: locale });
type ILocaleChartType = {
  ['agile.chart']: Array<keyof typeof locale>[number]
}
export { exportChart };
export type { ILocaleChartType };
