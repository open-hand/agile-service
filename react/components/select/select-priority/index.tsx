import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { fieldApi, priorityApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { Priority } from '@/common/types';
import { FlatSelect } from '@choerodon/components';

interface Props extends Partial<SelectProps> {
  priorityId?: number
  fieldId?: string
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (prioritys: Priority[]) => void
  flat?: boolean
  projectId?: string
  ruleIds?: string[]
  selected?: string[]
}

const SelectPriority: React.FC<Props> = forwardRef(({
  priorityId, fieldId, ruleIds, selected, dataRef, afterLoad, flat, projectId, ...otherProps
},
ref: React.Ref<Select>) => {
  const args = useMemo(() => ({ ruleIds, selected }), [ruleIds, selected]);
  const hasRule = Boolean(args);
  const config = useMemo((): SelectConfig => ({
    name: 'priority',
    textField: 'name',
    valueField: 'id',
    requestArgs: args,
    request: hasRule && fieldId
      ? ({ requestArgs, filter, page }) => fieldApi.getCascadeOptions(fieldId, requestArgs?.selected, requestArgs?.ruleIds, filter ?? '', page ?? 0, 0)
      : priorityApi.loadByProject(projectId, [String(priorityId)]),
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
    paging: hasRule,
  }), [afterLoad, args, dataRef, fieldId, hasRule, priorityId, projectId]);
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
