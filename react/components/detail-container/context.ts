import React, { createContext, useContext } from 'react';
import { DetailEvents } from '.';

export interface IRoute {
  path: string
  props?: Object
  events?: DetailEvents
}
export interface IRouteWithKey extends IRoute {
  key: React.Key
}
interface DetailContainerContext {
  outside: boolean
  topAnnouncementHeight: number
  routes: IRouteWithKey[]
  match: IRouteWithKey
  open: (route: IRoute) => void
  push: (nextRoute: IRoute) => void
  pop: () => void
  close: () => void
  eventsMap: Map<string, DetailEvents>
  fullPage?: boolean
  resizeRef: React.MutableRefObject<any>
}
const DetailContainerContext = createContext({} as DetailContainerContext);

function useDetailContainerContext() {
  const context = useContext(DetailContainerContext);
  return context;
}
export { useDetailContainerContext };
export default DetailContainerContext;
