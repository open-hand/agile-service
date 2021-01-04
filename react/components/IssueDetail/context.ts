import { createContext, useContext } from 'react';
import Store from './store';

interface Context {
  id: number
  store: Store
  topAnnouncementHeight: number
  projectId?: number
  hasAdminPermission: boolean
  disabledDetailEdit: boolean
  outside?: boolean
  organizationId?: string
  closeButton?: boolean
  applyType: string
}
const DetailContext = createContext({} as Context);

function useDetailContext() {
  const context = useContext(DetailContext);
  return context;
}
export { useDetailContext };
export default DetailContext;
