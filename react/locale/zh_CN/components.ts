import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {

} as const;
const exportComponents = localeAppendPrefixObjectKey({ intlPrefix: 'scrumBoard' as const, intlObject: locale });
type ILocaleScrumBoardType = {
  ['agile.components']: Array<keyof typeof locale>[number]
}
// export { exportComponents };
// export type { ILocaleScrumBoardType };
