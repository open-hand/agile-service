import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { priorityApi } from '@/api';
import { Priority } from '@/common/types';

export interface SelectWorkbenchPriorityProps extends Partial<SelectProps> {
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (priorities: Priority[]) => void
  flat?: boolean
  organizationId?: string
}

const SelectWorkbenchPriority: React.FC<SelectWorkbenchPriorityProps> = forwardRef(({
  dataRef, afterLoad, flat, organizationId, ...otherProps
},
ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'priority',
    textField: 'name',
    valueField: 'id',
    request: ({ filter }) => priorityApi.org(organizationId).loadWorkbench({ param: filter || '' }),
    middleWare: (data: Priority[]) => {
      const newData = data;
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
  }), [afterLoad, dataRef, organizationId]);
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
SelectWorkbenchPriority.displayName = 'SelectWorkbenchPriority';
export default SelectWorkbenchPriority;
