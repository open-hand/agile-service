import { createContext, useContext } from 'react';
import { getProjectId } from '@/utils/common';
import { commonApi } from '@/api';

const cache = new Map();
const IsOwnerContext = createContext({
  data: cache.get(getProjectId()) || false,
  refresh: async () => {
    const isOwner = await commonApi.getUserRolesInProject();
    cache.set(getProjectId(), isOwner);
    return isOwner;
  },
});
export { IsOwnerContext };

export default function useIsOwner() {
  const { data, refresh } = useContext(IsOwnerContext);
  return [data, refresh];
}
