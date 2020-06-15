import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { find } from 'lodash';
import { userApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';

interface User {
  email: string
  enabled?: boolean
  id: number
  imageUrl: string | null
  ldap: boolean
  loginName: string
  realName: string
}
interface Props {
  // 由于用户是分页的，有时候已选的用户不在第一页，这时候传id过来，会直接显示id，这里多传一个用户过来，放到options里
  selectedUser: User
}

const SelectUser: React.FC<Props> = forwardRef(({ selectedUser, ...otherProps }, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'user',
    textField: 'realName',
    valueField: 'id',
    request: ({ filter, page }) => userApi.getAllInProject(filter, page),
    middleWare: (data) => {
      if (selectedUser && selectedUser.id && !find(data, { id: selectedUser.id })) {
        return [selectedUser, ...data];
      } else {
        return data;
      }
    },
  }), [JSON.stringify(selectedUser)]);
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
