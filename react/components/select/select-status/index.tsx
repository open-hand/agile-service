import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { IStatusCirculation, statusTransformApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';

interface Props extends Partial<SelectProps> {
  issueTypeId?: string
  expectStatusId?: string
}

const SelectStatus: React.FC<Props> = forwardRef(
  ({ issueTypeId, expectStatusId, ...otherProps }, ref: React.Ref<Select>) => {
    const config = useMemo((): SelectConfig<IStatusCirculation> => ({
      name: 'status',
      textField: 'name',
      valueField: 'id',
      request: () => (issueTypeId ? statusTransformApi.loadList(issueTypeId) : Promise.resolve([])),
      middleWare: (statusList) => (
        expectStatusId
          ? statusList.filter(({ id }) => id !== expectStatusId)
          : statusList
      ),
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
