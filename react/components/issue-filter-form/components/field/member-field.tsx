import React, {
  useMemo, forwardRef, useState, useEffect,
} from 'react';
import { Select } from 'choerodon-ui/pro';
import { find, unionBy } from 'lodash';
import { commonApi, userApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import type { User } from '@/common/types';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { useMemberLocalMap } from '../../IssueFilterForm';
import { useIssueFilterFormStore } from '../../stores';

const toArray = (something: any) => (Array.isArray(something) ? something : [something]);
export interface SelectUserProps extends Partial<SelectProps> {
  // 由于用户是分页的，有时候已选的用户不在第一页，这时候传id过来，会直接显示id，这里多传一个用户过来，放到options里
  selectedUserIds?: Array<{ id: string }>,
  dataRef?: React.MutableRefObject<any>
  request?: SelectConfig<User>['request'],
  afterLoad?: (users: User[]) => void
}
const MemberField: React.FC<SelectUserProps> = forwardRef(({
  selectedUserIds, dataRef, request, afterLoad, ...otherProps
}, ref: React.Ref<Select>) => {
  const { noMemberLoadFinish, setNoMemberLoadFinish } = useIssueFilterFormStore();
  const [userMaps, addUser] = useMemberLocalMap({ events: { onFinish: (val) => setNoMemberLoadFinish(true) } });

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
      let newData: User[] = [];
      const sets = new Set(selectedUserIds?.map((item) => String(item.id)));
      newData = sets.size === 0 ? data.map((item) => ({ ...item, id: String(item.id) })) : data.map((item) => {
        const tempItem = { ...item, id: String(item.id) };
        if (Array.from(sets).some((id) => String(id) === tempItem.id)) {
          sets.delete(tempItem.id);
        }
        return tempItem;
      });
      const tempArr: User[] = [];
      if (newData.length > 0 && sets.size > 0) {
        const noUserIds = Array.from(sets);
        noUserIds.forEach((item: string) => {
          if (userMaps.userMaps.has(item)) {
            tempArr.push(userMaps.userMaps.get(item)!);
          } else {
            addUser(item);
          }
        });
      }
      return [...newData, ...tempArr];
    },
  }), [noMemberLoadFinish]);
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
export default MemberField;
