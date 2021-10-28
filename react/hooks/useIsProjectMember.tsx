import React, { useEffect, useState, useCallback } from 'react';
import { stores } from '@choerodon/boot';
import { userApi } from '@/api';
import { User } from '@/common/types';

const { AppState } = stores;

const useIsProjectMember = (projectId?: string) => {
  const [userInfo, setUserInfo] = useState<User>();
  const refresh = useCallback(async () => {
    const res = await userApi.project(projectId).getById(AppState.userInfo.id);
    if (res.list && res.list.length > 0) {
      setUserInfo(res.list[0]);
    }
  }, [projectId]);

  useEffect(() => {
    refresh();
  }, [refresh]);

  return { isProjectMember: !!userInfo, userInfo };
};
interface Props {
  children: (isProjectMember: boolean) => React.ReactElement,
  projectId?: string
}
const IsProjectMember: React.FC<Props> = ({ children, projectId }) => {
  const { isProjectMember } = useIsProjectMember(projectId);
  return children(isProjectMember);
};
export { IsProjectMember };
export default useIsProjectMember;
