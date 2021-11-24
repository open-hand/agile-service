import { localeAppendPrefixObjectKey } from '@/utils/locale';

const localeCommon = {
  field: '字段',
  priority: '优先级',
  description: '描述',
  default: '默认',
} as const;
const [intlPrefix, exportCommon] = localeAppendPrefixObjectKey({ intlPrefix: 'common' as const, intlObject: localeCommon });
export { exportCommon };
type ILocaleCommonType = {
  [intlPrefix]: Array<keyof typeof localeCommon>[number]
}
export type { ILocaleCommonType };
