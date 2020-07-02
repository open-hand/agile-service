import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { find } from 'lodash';
import { userApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import type { User } from '@/common/types';

const toArray = (something: any) => (Array.isArray(something) ? something : [something]);
interface Props {
  // 由于用户是分页的，有时候已选的用户不在第一页，这时候传id过来，会直接显示id，这里多传一个用户过来，放到options里
  selectedUser?: User | User[]
}

const SelectUser: React.FC<Props> = forwardRef(({ selectedUser, ...otherProps }, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<User> => ({
    name: 'user',
    textField: 'realName',
    valueField: 'id',
    request: ({ filter, page }) => userApi.getAllInProject(filter, page),
    middleWare: (data) => {
      if (selectedUser) {
        const temp: User[] = [];
        (toArray(selectedUser).forEach((user) => {
          if (!find(data, { id: user.id })) {
            temp.push(user);
          }
        }));
        return [...temp, ...data];
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
