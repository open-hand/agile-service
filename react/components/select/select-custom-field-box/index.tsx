import React, { useMemo, forwardRef } from 'react';
import { SelectBox } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { unionBy, castArray, partition } from 'lodash';
import { usePersistFn } from 'ahooks';
import { fieldApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
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
const SIZE = 0;
const SelectCustomField: React.FC<SelectCustomFieldProps> = forwardRef(({
  fieldId, fieldOptions, flat, projectId, afterLoad, afterFirstRequest, organizationId, selected, extraOptions, ruleIds, outside = false, onlyEnabled = true, menuType, ...otherProps
},
ref: React.Ref<SelectBox>) => {
  const args = useMemo(() => ({ ruleIds, selected }), [ruleIds, selected]);
  const hasRule = Object.keys(args).filter((key: keyof typeof args) => Boolean(args[key])).length > 0;
  const needOptions = useMemo(() => [...castArray(otherProps.value), ...castArray(selected)].filter(Boolean), [selected, otherProps.value]);
  const fakePageRequest = usePersistFn((filter: string = '', page: number = 1, size: number, ensureOptions: string[], enabled: boolean = true) => {
    if (!fieldOptions) {
      return [];
    }
    const [ensuredOptions, restOptions] = partition(fieldOptions, (item) => ensureOptions.includes(item.id));
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
    request: wrapRequestCallback(({ page, filter, requestArgs }) => {
      if (hasRule && fieldId) {
        return fieldApi.project(projectId).outside(outside).org(organizationId).getCascadeOptions(fieldId, requestArgs?.selected, requestArgs?.ruleIds, filter ?? '', page ?? 0, SIZE);
      }

      return fieldOptions ? fakePageRequest(filter, page, SIZE, needOptions, onlyEnabled) : fieldApi.outside(outside).org(organizationId).project(projectId).menu(menuType)
        .getFieldOptions(fieldId!, filter, page, SIZE, needOptions, onlyEnabled);
    }, (_, res) => {
      afterFirstRequest && afterFirstRequest(res);
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
  }), [args, hasRule, fieldId, fieldOptions, fakePageRequest, needOptions, onlyEnabled, outside, organizationId, projectId, menuType, afterFirstRequest, extraOptions, afterLoad]);
  const props = useSelect(config);
  return (
    <SelectBox
      ref={ref}
      {...props}
      {...otherProps}
      // @ts-ignore
      textField={null}
      valueField={null}
      searchable={false}
    />
  );
});
export default SelectCustomField;
