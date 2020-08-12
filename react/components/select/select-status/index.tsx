import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { statusTransformApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';

interface Props extends Partial<SelectProps>{
  issueTypeId?: string
}

const SelectStatus: React.FC<Props> = forwardRef(
  ({ issueTypeId, ...otherProps }, ref: React.Ref<Select>) => {
    const config = useMemo((): SelectConfig => ({
      name: 'status',
      textField: 'name',
      valueField: 'id',
      request: () => (issueTypeId ? statusTransformApi.loadList(issueTypeId) : Promise.resolve([])),
      paging: false,
    }), [issueTypeId]);
    const props = useSelect(config);
    return (
      <Select
        ref={ref}
        {...props}
        {...otherProps}
      />
    );
  },
);
export default SelectStatus;
