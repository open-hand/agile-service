import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { priorityApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { Priority } from '@/common/types';
import FlatSelect from '@/components/flat-select';

interface Props extends Partial<SelectProps> {
  priorityId?: number
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (prioritys: Priority[]) => void
  flat?: boolean
  projectId?: string
}

const SelectPriority: React.FC<Props> = forwardRef(({
  priorityId, dataRef, afterLoad, flat, projectId, ...otherProps
},
ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'priority',
    textField: 'name',
    valueField: 'id',
    // @ts-ignore
    request: () => priorityApi.loadByProject(projectId, [String(priorityId)]),
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
    paging: false,
  }), [priorityId]);
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
