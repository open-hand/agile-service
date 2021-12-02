import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  'collapse.complete.epic': 'Collapse Completed Epic',
  'hide.empty.story': 'Hide Empty Epics',
  'no.swimlane': 'No Swimlanes',
  'no.plan.list': 'Issues Without Epics',
  'version.swimlane': 'Version Swimlanes',
  'sprint.swimlane': 'Sprint Swimlanes',
  'empty.data.description': 'The story map is based on the epic, managed according to version control',
} as const;
const exportStoryMap = localeAppendPrefixObjectKey({ intlPrefix: 'storyMap' as const, intlObject: locale });
type ILocaleStoryMapType = {
  ['agile.storyMap']: Array<keyof typeof locale>[number]
}
export { exportStoryMap };
export type { ILocaleStoryMapType };
