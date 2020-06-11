import React, { useMemo } from 'react';
import { Select } from 'choerodon-ui/pro';
import { userApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';

interface Props {

}

const SelectUser: React.FC<Props> = (otherProps) => {
  const config = useMemo((): SelectConfig => ({
    name: 'user',
    textField: 'realName',
    valueField: 'id',
    request: ({ filter, page }) => userApi.getAllInProject(filter, undefined, page),
  }), []);
  const props = useSelect(config);
  return (
    <Select
      clearButton={false}
      {...props}
      {...otherProps}
    />
  );
};
export default SelectUser;
