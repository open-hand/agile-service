import { useFormatMessage as useFormat } from '@choerodon/master';
import type { COMMON_ZH_CN } from '@choerodon/master/lib/locale/zh_CN/common';
import type { IntlShape, MessageDescriptor } from 'react-intl';
import type {
  ILocaleBacklogType, ILocaleCommonType,
  ILocaleGanttType, ILocaleIssueType, ILocaleIssueTypeType,
  ILocalePageType, ILocaleProjectReportType, ILocaleScrumBoardType,
  ILocaleSettingType, ILocaleStateMachineType,
  ILocaleStoryMapType, ILocaleVersionType,
} from '../locale/zh_CN';

export type IAgilePrefixMapFormatMessageType = ILocaleBacklogType & ILocaleCommonType & ILocaleGanttType & ILocaleIssueType & ILocaleIssueTypeType &
  ILocalePageType & ILocaleProjectReportType & ILocaleScrumBoardType &
  ILocaleSettingType & ILocaleStateMachineType &
  ILocaleStoryMapType & ILocaleVersionType
type ILocaleBaseObject = { [key: string]: string }
export type IMasterLocaleCode = keyof typeof COMMON_ZH_CN
// @ts-ignore
export type KeysDistributeCodeObject<T extends ILocaleBaseObject, K extends keyof T = keyof T> = K extends string ? { code: `${K}.${T[K]}` } : never;
export type IAgilePrefixMapFormatMessageCode = KeysDistributeCodeObject<IAgilePrefixMapFormatMessageType>['code']
export type UseFormatMessageFunction<T extends string> = (descriptor: MessageDescriptor & { id: T }, values?: Parameters<IntlShape['formatMessage']>[1]) => ReturnType<IntlShape['formatMessage']>
export type IUseFormatMessageHook<T extends ILocaleBaseObject = IAgilePrefixMapFormatMessageType, C extends string = IAgilePrefixMapFormatMessageCode | IMasterLocaleCode> = {
  (): UseFormatMessageFunction<C>
  (intlPrefix: keyof T): UseFormatMessageFunction<T[keyof T]>
}
function useFormatMessage<T extends string = IAgilePrefixMapFormatMessageCode | IMasterLocaleCode>(): UseFormatMessageFunction<T>;
function useFormatMessage<T extends ILocaleBaseObject = IAgilePrefixMapFormatMessageType, K extends keyof T = keyof T>(intlPrefix: K): UseFormatMessageFunction<T[K]>;
function useFormatMessage<T extends ILocaleBaseObject = IAgilePrefixMapFormatMessageType, K extends keyof T = keyof T>(intlPrefix?: K): UseFormatMessageFunction<T[K]> {
  return useFormat(intlPrefix);
}
export default useFormatMessage;
