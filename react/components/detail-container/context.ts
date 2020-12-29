import { createContext, useContext } from 'react';

export interface IRoute {

}
interface DetailContainerContext {
  outside: boolean
  topAnnouncementHeight: number
  routes: IRoute[]
  match: IRoute
  push: (nextRoute: IRoute) => void
  pop: () => void
  close: () => void
  setVisible: (visible: boolean) => void
}
const DetailContainerContext = createContext({} as DetailContainerContext);

function useDetailContainerContext() {
  const context = useContext(DetailContainerContext);
  return context;
}
export { useDetailContainerContext };
export default DetailContainerContext;
