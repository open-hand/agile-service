import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  statistics: '个人工作项统计',
  incomplete: '未完成',
  complete: '已完成',
  issueCount: '工作项计数',
  workingHoursCount: '工时计数',
} as const;
const exportProjectOverview1 = localeAppendPrefixObjectKey({ intlPrefix: 'projectOverview1' as const, intlObject: locale });
type ILocaleProjectOverview = {
  ['agile.projectOverview1']: Array<keyof typeof locale>[number]
}
export { exportProjectOverview1 };
export type { ILocaleProjectOverview };
