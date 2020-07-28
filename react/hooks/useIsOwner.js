import { createContext, useContext } from 'react';
import { commonApi } from '@/api';

const IsOwnerContext = createContext({
  data: false,
  refresh: async () => {
    const isOwner = await commonApi.getUserRolesInProject();
    return isOwner;
  },
});
export { IsOwnerContext };

export default function useIsOwner() {
  const { data, refresh } = useContext(IsOwnerContext);  
  return [data, refresh];
}
