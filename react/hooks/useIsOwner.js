import { createContext, useContext } from 'react';
import { getIsOwner } from '@/api/CommonApi';

const IsOwnerContext = createContext({
  data: false,
  refresh: async () => {
    const isOwner = await getIsOwner();
    return isOwner;
  },
});
export { IsOwnerContext };

export default function useIsOwner() {
  const { data, refresh } = useContext(IsOwnerContext);  
  return [data, refresh];
}
