import { useFormatMessage as useFormat } from '@choerodon/master';
import type { IntlShape, MessageDescriptor } from 'react-intl';
import type { ILocaleCommonType } from '@/locale/zh_CN/common';

type PrefixMapFormatMessageCode = any
type KeyExtract<T, K extends keyof T = keyof T> = K extends string ? { intlPrefix: K, id: T[K] } : never
type UseFormatMessageFunction<T extends string> = (descriptor: MessageDescriptor & { id: T }, values?: Parameters<IntlShape['formatMessage']>[1]) => ReturnType<IntlShape['formatMessage']>
function useFormatMessage<T extends KeyExtract<PrefixMapFormatMessageCode> = KeyExtract<PrefixMapFormatMessageCode>>(intlPrefix?: T['intlPrefix']): UseFormatMessageFunction<string> {
  return useFormat(intlPrefix);
}

export default useFormatMessage;
