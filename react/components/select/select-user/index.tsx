import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { find } from 'lodash';
import { userApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import type { User } from '@/common/types';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';

const toArray = (something: any) => (Array.isArray(something) ? something : [something]);
export interface SelectUserProps extends Partial<SelectProps> {
  // 由于用户是分页的，有时候已选的用户不在第一页，这时候传id过来，会直接显示id，这里多传一个用户过来，放到options里
  selectedUser?: User | User[]
  extraOptions?: {
    id: string,
    realName: string,
  }[],
  dataRef?: React.MutableRefObject<any>
  request?: SelectConfig<User>['request']
  afterLoad?: (users: User[]) => void
}

const SelectUser: React.FC<SelectUserProps> = forwardRef(({
  selectedUser, extraOptions, dataRef, request, afterLoad, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<User> => ({
    name: 'user',
    textField: 'realName',
    valueField: 'id',
    request: request || (async ({ filter, page }) => {
      const res = await userApi.getAllInProject(filter, page);
      res.list = res.list.filter((user: User) => user.enabled);
      return res;
    }),
    middleWare: (data) => {
      let newData = [];
      if (selectedUser) {
        const temp: User[] = [];
        (toArray(selectedUser).forEach((user) => {
          if (!find(data, (item) => String(item.id) === user.id)) {
            temp.push(user);
          }
        }));
        newData = [...(extraOptions || []), ...temp, ...data].map((item: User) => (
          { ...item, id: item.id.toString() }
        ));
      } else {
        newData = [...(extraOptions || []), ...data].map((item: User) => (
          { ...item, id: item.id.toString() }
        ));
      }
      if (dataRef) {
        Object.assign(dataRef, {
          current: newData,
        });
      }
      if (afterLoad) {
        afterLoad(newData);
      }
      return newData;
    },
  }), [selectedUser]);
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
