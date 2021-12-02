import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  statistics: 'Member Workload Statistics',
  incomplete: 'Incomplete',
  complete: 'Complete',
  issueCount: 'Issue Count',
  workingHoursCount: 'Working Hours Count',
} as const;
const exportProjectOverview1 = localeAppendPrefixObjectKey({ intlPrefix: 'projectOverview1' as const, intlObject: locale });
type ILocaleProjectOverview = {
  ['agile.projectOverview1']: Array<keyof typeof locale>[number]
}
export { exportProjectOverview1 };
export type { ILocaleProjectOverview };
