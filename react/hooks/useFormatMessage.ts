import { useFormatMessage as useFormat } from '@choerodon/master';
import type { IntlShape, MessageDescriptor } from 'react-intl';
import type { localeCommon } from '@/locale/zh_CN';

interface PrefixMapFormatMessageCode {
    'common': typeof localeCommon
}
type KeyExtract<T, K extends keyof T = keyof T> = K extends string ? { intlPrefix: K, id: keyof T[K] } : never
type UseFormatMessageFunction<T extends string> = (descriptor: { id: T }) => ReturnType<IntlShape['formatMessage']>

function useFormatMessage<T extends KeyExtract<PrefixMapFormatMessageCode>=KeyExtract<PrefixMapFormatMessageCode>>(intlPrefix: T['intlPrefix']):UseFormatMessageFunction<T['id']> {
  return useFormat(intlPrefix);
}
export default useFormatMessage;
