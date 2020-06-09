import React from 'react';
import { Select } from 'choerodon-ui/pro';
import useSelect, { SelectType } from '@/hooks/useSelect';

const SelectUser: React.FC = (otherProps) => {
  const props = useSelect(SelectType.user);
  return (
    <Select
      {...props}
      {...otherProps}
    />
  );
};
export default SelectUser;
