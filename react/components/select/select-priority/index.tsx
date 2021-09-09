import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { fieldApi, priorityApi } from '@/api';
import { Priority } from '@/common/types';

export interface SelectPriorityProps extends Partial<SelectProps> {
  priorityId?: number
  fieldId?: string
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (prioritys: Priority[]) => void
  flat?: boolean
  projectId?: string
  ruleIds?: string[]
  selected?: string[]
}

const SelectPriority: React.FC<SelectPriorityProps> = forwardRef(({
  priorityId, fieldId, ruleIds, selected, dataRef, afterLoad, flat, projectId, ...otherProps
},
ref: React.Ref<Select>) => {
  const args = useMemo(() => ({ ruleIds, selected }), [ruleIds, selected]);
  const hasRule = Object.keys(args).filter((key: keyof typeof args) => Boolean(args[key])).length > 0;
  const isRequestCascadeOptions = !!(hasRule && fieldId);
  const config = useMemo((): SelectConfig => ({
    name: 'priority',
    textField: 'name',
    valueField: 'id',
    requestArgs: args,
    request: isRequestCascadeOptions
      ? ({ requestArgs, filter, page }) => fieldApi.project(projectId).getCascadeOptions(fieldId!, requestArgs?.selected, requestArgs?.ruleIds, filter ?? '', page ?? 0, 0)
      : () => priorityApi.loadByProject(projectId, [String(priorityId)]),
    middleWare: (data: Priority[]) => {
      if (dataRef) {
        Object.assign(dataRef, {
          current: data,
        });
      }
      if (afterLoad) {
        afterLoad(data);
      }
      return data;
    },
    paging: isRequestCascadeOptions,
  }), [afterLoad, args, dataRef, fieldId, isRequestCascadeOptions, priorityId, projectId]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={ref}
      clearButton={false}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectPriority;
