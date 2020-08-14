import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { statusApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IStatus } from '@/common/types';

interface Props extends Partial<SelectProps> {
  expectStatusIds?: string[]
}

const SelectStatus: React.FC<Props> = forwardRef(({
  expectStatusIds,
  ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<IStatus> => ({
    name: 'status',
    textField: 'name',
    valueField: 'id',
    request: () => statusApi.loadByProject(),
    middleWare: (statusList) => (expectStatusIds && expectStatusIds.length > 0
      ? statusList.filter((status) => !expectStatusIds.includes(status.id))
      : statusList),
    paging: false,
  }), []);
  const props = useSelect(config);
  return (
    <Select
      ref={ref}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectStatus;
