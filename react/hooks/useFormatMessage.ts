import { useFormatMessage as useFormat } from '@choerodon/master';
import type { IntlShape, MessageDescriptor } from 'react-intl';
import type { ILocaleCommonType } from '@/locale/zh_CN/common';

type PrefixMapFormatMessageCode = ILocaleCommonType
type KeyExtract<T, K extends keyof T = keyof T> = K extends string ? { intlPrefix: K, id: T[K] } : never
type UseFormatMessageFunction<T extends string> = (descriptor: MessageDescriptor & { id: T }) => ReturnType<IntlShape['formatMessage']>
// console.log('useFormat', useFormat);
function useFormatMessage<T extends KeyExtract<PrefixMapFormatMessageCode> = KeyExtract<PrefixMapFormatMessageCode>>(intlPrefix?: T['intlPrefix']): UseFormatMessageFunction<string> {
  return useFormat(intlPrefix);
}

export default useFormatMessage;
