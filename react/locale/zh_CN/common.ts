import { localeAppendPrefixObjectKey } from '@/utils/locale';

const intlPrefix = 'common' as const;
const localeCommon = {
  field: '字段',
  priority: '优先级',
  description: '描述',
  default: '默认',
} as const;
const exportCommon = localeAppendPrefixObjectKey({ intlPrefix, intlObject: localeCommon });
export { exportCommon };
export type { localeCommon, intlPrefix };
