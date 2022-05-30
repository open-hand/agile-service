import { MutableRefObject, useMemo, useRef } from 'react';
import { omit } from 'lodash';
import { useCreation } from 'ahooks';
import { LoadConfig, SelectConfig } from '@/hooks/useSelect';
import { refsBindRef } from './utils';
import useDeepCompareCreation from '@/hooks/useDeepCompareCreation';
import useSelectRequestArgsValue from './useSelectRequestArgsValue';

export interface IHookSelectWithRuleOptions {
  selected?: string | string[]
  ruleIds?: string[]
  fieldId?: string
  /** 是否禁用级联规则相关配置
   *
   *  @default false
   * */
  disabledRuleConfig?: boolean
}
export type IHookSelectRuleRequestArgs = Pick<IHookSelectWithRuleOptions, 'fieldId' | 'ruleIds'> & { hasRule: boolean, selected?: string[] }
export type LoadConfigWithRule = Omit<LoadConfig, 'requestArgs'> & {
  requestArgs: IHookSelectRuleRequestArgs & {
    [key: string]: any
  }
}
export type SelectConfigWithRule<T = {}> = Omit<SelectConfig<T>, 'request' | 'middleWare'> & {
  request: ({ filter, page }: LoadConfigWithRule) => Promise<T[] | {
    list: T[];
    hasNextPage: boolean;
  }>;
  middleWare?: (data: T[], requestArgs: IHookSelectRuleRequestArgs) => T[]
}

/**
 * useSelect的辅助hook 会自动注入args 配置
 * @param config
 */
function useSelectWithRuleConfig<T extends {}>(config: SelectConfigWithRule<T>, {
  selected: propsSelected, ruleIds, fieldId, disabledRuleConfig,
}: IHookSelectWithRuleOptions = {}) {
  const { requestArgs } = config;
  const ruleBaseRequestArg = useMemo(() => ({ hasRule: false, fieldId }), [fieldId]);
  const hasRule = !!(!disabledRuleConfig && ruleBaseRequestArg.fieldId && (ruleIds?.length || propsSelected?.length));
  const dataRef = useRef<(data:any[])=>void>();
  const requestArgsOmit = omit(requestArgs, ['selected', 'ruleIds']);
  const selected = useSelectRequestArgsValue({ dataRef, value: propsSelected });
  const requestArgsConfig = useDeepCompareCreation(() => {
    if (hasRule) {
      return {
        ...requestArgsOmit, ...ruleBaseRequestArg, ruleIds, hasRule: true, selected,
      };
    }
    return ruleBaseRequestArg;
  }, [requestArgsOmit, hasRule, ruleIds, selected, ruleBaseRequestArg]);
  const dataFnRef = useRef<any>();
  dataFnRef.current = refsBindRef(dataRef.current, config.dataRef);
  const configWithRuleConfig = useCreation(() => ({
    ...config,
    dataRef: dataFnRef,
    middleWare: config.middleWare && ((data) => config.middleWare!(data, requestArgsConfig as any)),
    paging: requestArgsConfig.hasRule ? true : config.paging,
    requestArgs: requestArgsConfig,
  } as SelectConfig<T>), [config, requestArgsConfig]);
  return configWithRuleConfig;
}

export default useSelectWithRuleConfig;
