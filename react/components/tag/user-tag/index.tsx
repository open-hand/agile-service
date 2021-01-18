import React, { useMemo } from 'react';
import { toJS } from 'mobx';
import { User } from '@/common/types';

import UserHead from '@/components/UserHead';
import Users from '../users';
/**
 * 数组内只有一个User 或者User是对象时,显示名字
 */
interface Props {
    data: User[] | User /**   */
    maxTagCount?: number /** @default 3 */
}
const UserTag: React.FC<Props> = ({ data: propsData, maxTagCount }) => {
  const data = useMemo(() => {
    const newData = toJS(propsData);
    if (!newData) {
      return [];
    }
    if (Array.isArray(newData) && newData.length === 1) {
      return newData[0];
    }
    return newData;
  }, [propsData]);
  return Array.isArray(data) ? <Users data={data} maxTagCount={maxTagCount} /> : <UserHead user={data} />;
};
export default UserTag;
