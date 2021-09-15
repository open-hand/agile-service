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
export interface IPreview {
  url: string
  name: string
}
interface DetailContainerContext {
  descriptionChanged: boolean
  setDescriptionChanged: (changed: boolean) => void
  outside: boolean
  /** css 变量的高度  */
  topAnnouncementHeight: string
  routes: IRouteWithKey[]
  match: IRouteWithKey
  open: (route: IRoute) => void
  push: (nextRoute: IRoute) => void
  pop: () => void
  close: () => void
  eventsMap: Map<string, DetailEvents>
  fullPage?: boolean
  resizeRef: React.MutableRefObject<any>
  filePreview?: IPreview
  setFilePreview: (filePreview?: IPreview) => void
  hidden: boolean
  setHidden: (hidden: boolean) => void
}
const DetailContainerContext = createContext({} as DetailContainerContext);

function useDetailContainerContext() {
  const context = useContext(DetailContainerContext);
  return context;
}
export { useDetailContainerContext };
export default DetailContainerContext;
