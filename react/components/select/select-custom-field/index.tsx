import React, {
  useMemo, forwardRef, useCallback, useRef,
} from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { unionBy, castArray, partition } from 'lodash';
import { usePersistFn } from 'ahooks';
import { fieldApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import useSelectWithRuleConfig, { SelectConfigWithRule } from '../useSelectWithRuleConfig';
import { wrapRequestCallback } from '../utils';

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
  fieldOptions, flat, afterLoad, afterFirstRequest, projectId, organizationId, disabledRuleConfig, selected: propsSelected, extraOptions, ruleIds, outside = false, onlyEnabled = true, menuType, fieldId: propsFieldId, ...otherProps
},
ref: React.Ref<Select>) => {
  const args = useMemo(() => ({ fieldOptions }), [fieldOptions]);
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
  const getRequest: SelectConfigWithRule<any>['request'] = usePersistFn(async ({ page, filter, requestArgs }) => {
    const { fieldId } = requestArgs;
    if (requestArgs.hasRule) {
      if (!fieldId) {
        return new Promise(() => ({ content: [], list: [], emptyData: true }));
      }
      return fieldApi.org(organizationId).project(projectId).outside(outside).getCascadeOptions(fieldId, requestArgs?.selected || [], requestArgs?.ruleIds || [], filter ?? '', page ?? 0, SIZE);
    }

    const needOptions = [...castArray(otherProps.value), ...(requestArgs.selected || [])].filter(Boolean);
    if (requestArgs.fieldOptions) {
      return fakePageRequest(filter, page, SIZE, needOptions, onlyEnabled, requestArgs?.fieldOptions);
    }
    if (fieldId) {
      return fieldApi.outside(outside).org(organizationId).project(projectId).menu(menuType)
        .getFieldOptions(fieldId!, filter, page, SIZE, needOptions, onlyEnabled);
    }

    return new Promise(() => ({ content: [], list: [], emptyData: true }));
  });
  const configWithRule = useMemo((): SelectConfigWithRule<any> => ({
    textField: 'value',
    valueField: 'id',
    requestArgs: args,
    tooltip: true,
    request: wrapRequestCallback(getRequest, async (_, res, originRes) => {
      if (!originRes.emptyData && afterFirstRequest) {
        afterFirstRequest(res);
      }
    }),
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
  }), [args, getRequest, afterFirstRequest, extraOptions, afterLoad]);
  const config = useSelectWithRuleConfig(configWithRule, { selected: propsSelected, ruleIds, fieldId: propsFieldId });
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
