import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { filter } from 'lodash';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { fieldApi, priorityApi } from '@/api';
import { Priority } from '@/common/types';
import { wrapRequestCallback } from '../utils';

export interface SelectPriorityProps extends Partial<SelectProps> {
  priorityId?: number
  fieldId?: string
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (prioritys: Priority[]) => void
  /** 首次请求结束后 */
  afterFirstRequest?: SelectConfig['afterLoad'],
  flat?: boolean
  projectId?: string
  ruleIds?: string[]
  selected?: string[]
}

const SelectPriority: React.FC<SelectPriorityProps> = forwardRef(({
  priorityId, fieldId, ruleIds, selected, dataRef, afterLoad, afterFirstRequest, flat, projectId, ...otherProps
},
ref: React.Ref<Select>) => {
  const args = useMemo(() => ({ ruleIds, selected }), [ruleIds, selected]);
  const hasRule = !!(!!ruleIds?.length && fieldId);

  const config = useMemo((): SelectConfig => ({
    name: 'priority',
    textField: 'name',
    valueField: 'id',
    requestArgs: args,
    request: wrapRequestCallback(hasRule
      ? ({ requestArgs, filter: param, page }) => fieldApi.project(projectId).getCascadeOptions(fieldId!, requestArgs?.selected, requestArgs?.ruleIds, param ?? '', page ?? 0, 0)
      : () => priorityApi.loadByProject(projectId, [String(priorityId)]), (_, res) => {
      afterFirstRequest && afterFirstRequest(res);
    }),
    middleWare: (data: Priority[]) => {
      const newData = hasRule ? data : filter(data, (item) => item.enable || item.id === String(priorityId));
      if (dataRef) {
        Object.assign(dataRef, {
          current: newData,
        });
      }
      if (afterLoad) {
        afterLoad(newData);
      }
      return newData;
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
