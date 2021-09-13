import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { uniqBy } from 'lodash';
import { componentApi, fieldApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { IComponent } from '@/common/types';

export interface SelectComponentProps extends Partial<SelectProps> {
  dataRef?: React.MutableRefObject<any>
  valueField?: string
  afterLoad?: (components: IComponent[]) => void
  flat?: boolean
  projectId?: string
  extraOptions?: IComponent[]
  ruleIds?: string[]
  selected?: string[]
  fieldId?: string
}

const SelectComponent: React.FC<SelectComponentProps> = forwardRef(({
  dataRef, afterLoad, valueField, flat, projectId, extraOptions, ruleIds, selected, fieldId, ...otherProps
}, ref: React.Ref<Select>) => {
  const args = useMemo(() => ({ ruleIds, selected }), [ruleIds, selected]);
  const hasRule = Object.keys(args).filter((key: keyof typeof args) => Boolean(args[key])).length > 0;
  const config = useMemo((): SelectConfig<IComponent> => ({
    name: 'component',
    textField: 'name',
    valueField: valueField || 'componentId',
    requestArgs: args,
    request: hasRule && fieldId
      ? ({ requestArgs, filter, page }) => fieldApi.project(projectId).getCascadeOptions(fieldId, requestArgs?.selected, requestArgs?.ruleIds, filter ?? '', page ?? 0, 50)
      : ({ page, filter }) => componentApi.loadAllComponents(filter, projectId, page, 50),
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
      return data;
    },
    paging: true,
    tooltip: true,
  }), [valueField, args, hasRule, fieldId, projectId, dataRef, extraOptions, afterLoad]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      ref={ref}
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
