import { useFormatMessage as useFormat } from '@choerodon/master';
import type { COMMON_ZH_CN } from '@choerodon/master/lib/locale/zh_CN/common';
import type { IntlShape, MessageDescriptor } from 'react-intl';
import type {
  ILocaleBacklogType, ILocaleCommonType,
  ILocaleGanttType, ILocaleIssueType, ILocaleIssueTypeType,
  ILocalePageType, ILocaleProjectReportType, ILocaleScrumBoardType,
  ILocaleSettingType, ILocaleStateMachineType,
  ILocaleStoryMapType, ILocaleVersionType,
} from '@/locale/zh_CN';

type IAgilePrefixMapFormatMessageCode = ILocaleBacklogType & ILocaleCommonType & ILocaleGanttType & ILocaleIssueType & ILocaleIssueTypeType &
  ILocalePageType & ILocaleProjectReportType & ILocaleScrumBoardType &
  ILocaleSettingType & ILocaleStateMachineType &
  ILocaleStoryMapType & ILocaleVersionType
type ILocaleBaseObject = { [key: string]: string }
type IMasterLocaleCode = keyof typeof COMMON_ZH_CN
// @ts-ignore
type KeysDistributeObject<T extends ILocaleBaseObject, K extends keyof T = keyof T> = K extends string ? { code: `${K}.${T[K]}` } : never;

export type UseFormatMessageFunction<T extends string> = (descriptor: MessageDescriptor & { id: T }, values?: Parameters<IntlShape['formatMessage']>[1]) => ReturnType<IntlShape['formatMessage']>
function useFormatMessage(): UseFormatMessageFunction<KeysDistributeObject<IAgilePrefixMapFormatMessageCode>['code'] | IMasterLocaleCode>;
function useFormatMessage<T extends ILocaleBaseObject = IAgilePrefixMapFormatMessageCode, K extends keyof T = keyof T>(intlPrefix: K): UseFormatMessageFunction<T[K]>;
function useFormatMessage<T extends ILocaleBaseObject = IAgilePrefixMapFormatMessageCode, K extends keyof T = keyof T>(intlPrefix?: K): UseFormatMessageFunction<T[K]> {
  return useFormat(intlPrefix);
}
export default useFormatMessage;
