import React, { useMemo, forwardRef, useCallback } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { unionBy, castArray, partition } from 'lodash';
import { usePersistFn } from 'ahooks';
import { fieldApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';

export interface SelectCustomFieldBasicProps extends Partial<SelectProps> {
  selected?: string[] | string
  extraOptions?: any[]
  flat?: boolean
  projectId?: string
  organizationId?: string
  outside?: boolean
  ruleIds?: string[]
  menuType?: 'project' | 'organization'
  /** 是否禁用级联规则相关配置 @default false */
  disabledRuleConfig?: boolean
  afterLoad?: (data: any) => void
  /** 首次请求结束后 */
  afterFirstRequest?: SelectConfig['afterLoad'],
}
// 参数互斥，要么传fieldId，要么传fieldOptions
export type SelectCustomFieldProps = SelectCustomFieldBasicProps & ({
  fieldId: string
  fieldOptions?: never
  onlyEnabled?: boolean
} | {
  fieldId?: never
  // 把所有options传进来，前端假分页
  fieldOptions: {
    id: string
    value: string
    enabled: boolean
  }[]
  onlyEnabled?: boolean
})
const SIZE = 50;
const SelectCustomField: React.FC<SelectCustomFieldProps> = forwardRef(({
  fieldId, fieldOptions, flat, afterLoad, afterFirstRequest, projectId, organizationId, disabledRuleConfig, selected, extraOptions, ruleIds, outside = false, onlyEnabled = true, menuType, ...otherProps
},
ref: React.Ref<Select>) => {
  const args = useMemo(() => ({ ruleIds, fieldOptions, selected: selected ? castArray(selected).filter(Boolean) : undefined }), [fieldOptions, ruleIds, selected]);
  const hasRule = !disabledRuleConfig && Object.keys(args).filter((key: keyof typeof args) => key !== 'fieldOptions' && Boolean(args[key])).length > 0;
  const needOptions = useMemo(() => [...castArray(otherProps.value), ...(args.selected || [])].filter(Boolean), [otherProps.value, args.selected]);
  const fakePageRequest = usePersistFn((filter: string = '', page: number = 1, size: number, ensureOptions: string[], enabled: boolean = true, optionData: any = undefined) => {
    if (!optionData) {
      return [];
    }
    const [ensuredOptions, restOptions] = partition(optionData, (item) => ensureOptions.includes(item.id));
    const list = restOptions.filter((item) => (enabled ? item.enabled : true)).filter((item) => item.value && item.value?.indexOf(filter) > -1).slice(Math.max((page - 1) * size, 0), (page) * size);
    return {
      // 第一页包含已选的选项
      list: page === 1 ? [...ensuredOptions, ...list] : list,
      hasNextPage: restOptions.length > (page) * size,
    };
  });
  const config = useMemo((): SelectConfig => ({
    textField: 'value',
    valueField: 'id',
    requestArgs: args,
    tooltip: true,
    request: async ({ page, filter, requestArgs }) => {
      let request = () => (requestArgs?.fieldOptions ? fakePageRequest(filter, page, SIZE, needOptions, onlyEnabled, requestArgs?.fieldOptions) : fieldApi.outside(outside).org(organizationId).project(projectId).menu(menuType)
        .getFieldOptions(fieldId!, filter, page, SIZE, needOptions, onlyEnabled));
      if (hasRule) {
        request = () => (!fieldId ? new Promise(() => ({ content: [], list: [], emptyData: true })) : fieldApi.org(organizationId).project(projectId).outside(outside).getCascadeOptions(fieldId, requestArgs?.selected, requestArgs?.ruleIds, filter ?? '', page ?? 0, SIZE));
      }
      const res = await request();
      if (!res.emptyData && afterFirstRequest) {
        afterFirstRequest(res.content);
      }
      return res;
    },
    middleWare: (data) => {
      if (!extraOptions) {
        if (afterLoad) {
          afterLoad(data);
        }
        return data;
      }
      const res = unionBy([...extraOptions, ...data], 'id');
      if (afterLoad) {
        afterLoad(res);
      }
      return res;
    },
    paging: true,
  }), [args, hasRule, afterFirstRequest, fakePageRequest, needOptions, onlyEnabled, outside, organizationId, projectId, menuType, fieldId, extraOptions, afterLoad]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={ref}
      {...props}
      {...otherProps}
      // @ts-ignore
      textField={null}
      valueField={null}
    />
  );
});
export default SelectCustomField;
