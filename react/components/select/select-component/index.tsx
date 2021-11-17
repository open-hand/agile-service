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
import { useNoticeSelectUpdateSelected } from '../useNoticeSelectUpdateSelected';
import { refsBindRef, wrapRequestCallback } from '../utils';

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
  fieldId?: string
}
function getSelectIds(options: any[], values: any, valueField?: string) {
  if (valueField === 'name') {
    return castArray(values || []).map((v) => options.find((option) => option.name === v)?.componentId);
  }
  return values || [];
}
const SelectComponent: React.FC<SelectComponentProps> = forwardRef(({
  dataRef, afterLoad, valueField, flat, projectId, extraOptions, ruleIds, selected, fieldId, name, defaultSelectedIds: propsDefaultSelectedIds, ...otherProps
}, ref: React.Ref<Select>) => {
  const selectRef = useRef<Select>();
  const optionsRef = useRef<any[]>(extraOptions || []);
  const values = useComputed(() => ((castArray(otherProps.value ?? (selectRef.current?.getValues() || []))).flat(Infinity).map((item: any) => (typeof item === 'object' ? get(item, 'componentId') : item))), [otherProps.value, selectRef.current?.getValues()]);
  const defaultSelectedIds = useCreation(() => (castArray(toJS(propsDefaultSelectedIds))), []);
  const selectIdsRef = useRef<string[] | undefined>(defaultSelectedIds);
  const [forceValue, setFilterWord] = useNoticeSelectUpdateSelected();
  const selectIds = useCreation(() => {
    if (optionsRef.current) {
      const idValues = getSelectIds(optionsRef.current || [], values, valueField);
      const hasNewUnExistValue = idValues.some((v: string) => !optionsRef.current?.find((item) => item.componentId === v));
      if (hasNewUnExistValue || forceValue || !selectIdsRef.current) {
        selectIdsRef.current = idValues;
      }
    }
    return [...castArray(selectIdsRef.current || [])].filter(Boolean);
  }, [forceValue, values, valueField]);
  const ruleArgs = useMemo(() => ({ ruleIds, selected }), [ruleIds, selected]);

  const hasRule = Object.keys(ruleArgs).filter((key: keyof typeof ruleArgs) => Boolean(ruleArgs[key])).length > 0;
  const args = useMemo(() => ({ ...ruleArgs, selectIds }), [ruleArgs, selectIds]);
  const config = useMemo((): SelectConfig<IComponent> => ({
    name: 'component',
    textField: 'name',
    valueField: valueField || 'componentId',
    requestArgs: args,
    request: wrapRequestCallback(hasRule && fieldId
      ? ({ requestArgs, filter, page }) => fieldApi.project(projectId).getCascadeOptions(fieldId, requestArgs?.selected, requestArgs?.ruleIds, filter ?? '', page ?? 0, 50)
      : ({ page, filter, requestArgs }) => componentApi.loadAllComponents(filter, projectId, page, 50, requestArgs?.selectIds),
    ({ filter }) => setFilterWord('filter', filter)),
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
  }), [valueField, args, hasRule, fieldId, projectId, setFilterWord, dataRef, extraOptions, afterLoad]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;

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
