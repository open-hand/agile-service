import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { userApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';

interface Props {

}

const SelectUser: React.FC<Props> = forwardRef((otherProps, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'user',
    textField: 'realName',
    valueField: 'id',
    request: ({ filter, page }) => userApi.getAllInProject(filter, undefined, page),
  }), []);
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
export default SelectUser;
