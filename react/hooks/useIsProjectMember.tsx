import React, { useEffect, useState, useCallback } from 'react';
import { stores } from '@choerodon/boot';
import { userApi } from '@/api';

const { AppState } = stores;

const useIsProjectMember = () => {
  const [isProjectMember, setIsProjectMember] = useState<boolean>(false);
  const refresh = useCallback(async () => {
    const res = await userApi.getById(AppState.userInfo.id);
    if (res.list && res.list.length > 0) {
      setIsProjectMember(true);
    }
  }, []);

  useEffect(() => {
    refresh();
  }, [refresh]);

  return isProjectMember;
};
interface Props {
  children: (isProjectMember: boolean) => React.ReactElement,
}
const IsProjectMember: React.FC<Props> = ({ children }) => {
  const isProjectMember = useIsProjectMember();
  return children(isProjectMember);
};
export { IsProjectMember };
export default useIsProjectMember;
