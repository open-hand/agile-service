import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { priorityApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { Priority } from '@/common/types';

interface Props extends Partial<SelectProps> {
  priorityId?: number
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (prioritys: Priority[]) => void
}

const SelectPriority: React.FC<Props> = forwardRef(({
  priorityId, dataRef, afterLoad, ...otherProps
},
ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'priority',
    textField: 'name',
    valueField: 'id',
    request: () => priorityApi.loadByProject(undefined, [String(priorityId)]),
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
  return (
    <Select
      ref={ref}
      clearButton={false}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectPriority;
