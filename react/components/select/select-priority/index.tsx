import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { filter } from 'lodash';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { fieldApi, priorityApi } from '@/api';
import { Priority } from '@/common/types';
import useSelectWithRuleConfig, { SelectConfigWithRule } from '../useSelectWithRuleConfig';

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
  priorityId, dataRef, afterLoad, afterFirstRequest, flat, projectId, ...otherProps
},
ref: React.Ref<Select>) => {
  const configWithRule = useMemo((): SelectConfigWithRule => ({
    name: 'priority',
    textField: 'name',
    valueField: 'id',
    request: ({ requestArgs, filter: param, page }) => (requestArgs.hasRule ? fieldApi.project(projectId).getCascadeOptions(requestArgs.fieldId!, requestArgs?.selected || [], requestArgs?.ruleIds || [], param ?? '', page ?? 0, 0)
      : priorityApi.loadByProject(projectId, [String(priorityId)])),
    afterLoad: afterFirstRequest,
    middleWare: (data: Priority[], requestArgs) => {
      const newData = requestArgs.hasRule ? data : filter(data, (item) => item.enable || item.id === String(priorityId));
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
    paging: false,
  }), [afterFirstRequest, afterLoad, dataRef, priorityId, projectId]);
  const config = useSelectWithRuleConfig(configWithRule, otherProps);
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
