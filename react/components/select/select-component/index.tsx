import React, {
  useMemo, forwardRef, useRef, useEffect,
} from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import {
  castArray, get, set, uniqBy,
} from 'lodash';
import { toJS } from 'mobx';
import { useComputed } from 'mobx-react-lite';
import { useCreation } from 'ahooks';
import { componentApi, fieldApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { IComponent } from '@/common/types';
import { refsBindRef } from '../utils';
import useSelectWithRuleConfig, { SelectConfigWithRule } from '../useSelectWithRuleConfig';

export interface SelectComponentProps extends Partial<SelectProps> {
  dataRef?: React.MutableRefObject<any>
  valueField?: string
  afterLoad?: (components: IComponent[]) => void
  flat?: boolean
  projectId?: string
  extraOptions?: IComponent[]
  ruleIds?: string[]
  selected?: string[]
  /** 默认选中的项 仅首次装载的使用 */
  defaultSelectedIds?: string[]
  /** 首次请求结束后 */
  afterFirstRequest?: SelectConfig['afterLoad'],
  fieldId?: string
}
function getSelectIds(options: any[], values: any, valueField?: string) {
  if (valueField === 'name') {
    return castArray(values || []).map((v) => options.find((option) => option.name === v)?.componentId);
  }
  return values || [];
}
const SelectComponent: React.FC<SelectComponentProps> = forwardRef(({
  dataRef, afterLoad, afterFirstRequest, valueField, flat, projectId, extraOptions, ruleIds, selected: propsSelected, fieldId, name, defaultSelectedIds: propsDefaultSelectedIds, ...otherProps
}, ref: React.Ref<Select>) => {
  const selectRef = useRef<Select>();
  const optionsRef = useRef<any[]>(extraOptions || []);
  const values = useComputed(() => ((castArray(otherProps.value ?? (selectRef.current?.getValues() || []))).flat(Infinity).map((item: any) => (typeof item === 'object' ? get(item, 'componentId') : item))), [otherProps.value, selectRef.current?.getValues()]);
  const selectIds = useCreation(() => {
    let newSelectIds: string[] = propsDefaultSelectedIds || [];
    if (optionsRef.current) {
      newSelectIds = getSelectIds(optionsRef.current || [], values, valueField);
    }
    newSelectIds.push(...castArray(propsSelected).map((item: any) => (typeof item === 'object' ? item.componentId : item)));
    return newSelectIds.filter(Boolean);
  }, [values, valueField, propsSelected]);

  const configWithRule = useMemo((): SelectConfigWithRule<IComponent> => ({
    name: 'component',
    textField: 'name',
    valueField: valueField || 'componentId',
    request: ({ requestArgs, filter, page }) => (requestArgs.hasRule ? fieldApi.project(projectId)
      .getCascadeOptions(requestArgs.fieldId, requestArgs.selected, requestArgs.ruleIds, filter ?? '', page ?? 0, 50)
      : componentApi.loadAllComponents(filter, projectId, page, 50, requestArgs.selected)),
    afterLoad: afterFirstRequest,
    middleWare: (components) => {
      // @ts-ignore
      let data = components || [];
      if (dataRef) {
        Object.assign(dataRef, {
          current: data,
        });
      }
      if (extraOptions) {
        extraOptions.forEach((item) => {
          data.push(item);
        });
      }
      data = uniqBy(data, 'componentId');
      if (afterLoad) {
        afterLoad(data);
      }
      optionsRef.current = data;
      return data;
    },
    paging: true,
    tooltip: true,
  }), [valueField, afterFirstRequest, projectId, dataRef, extraOptions, afterLoad]);
  console.log('ruleIds', ruleIds);
  const config = useSelectWithRuleConfig(configWithRule, { ruleIds, selected: selectIds, fieldId });
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;
  if (selectRef.current && name) {
    selectRef.current?.dataSet?.setState(`${name}-options`, props.options);
  }

  return (
    <Component
      ref={refsBindRef(ref, selectRef)}
      name={name}
      clearButton
      multiple
      popupStyle={{ maxWidth: '3rem !important' }}
      maxTagTextLength={10}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectComponent;
