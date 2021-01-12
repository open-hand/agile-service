import React, {
  useCallback, useState, useEffect, useRef,
} from 'react';
import { Animate } from 'choerodon-ui';
import { stores } from '@choerodon/boot';
import Container, { registerPath } from './Container';
import DetailContainerContext, { IRoute } from './context';
// 默认展示children，push之后再匹配
const { HeaderStore } = stores;
export { registerPath };
export interface DetailEvents {
  [type: string]: () => void
}
export const useDetail = (): [DetailContainerProps] => {
  const [routes, setRoutes] = useState<IRoute[]>([]);
  const [visible, setVisible] = useState(false);
  const eventsMap = useRef<Map<string, DetailEvents>>(new Map());
  const updateEventsMap = useCallback((path: string, events?: DetailEvents) => {
    if (events) {
      eventsMap.current.set(path, events);
    }
  }, []);
  const match = routes[routes.length - 1];
  const push = useCallback((nextRoute: IRoute) => {
    setRoutes((r) => ([...r, nextRoute]));
    updateEventsMap(nextRoute.path, nextRoute.events);
    setVisible(true);
  }, [updateEventsMap]);
  const open = useCallback((route: IRoute) => {
    setRoutes([route]);
    updateEventsMap(route.path, route.events);
    setVisible(true);
  }, [updateEventsMap]);
  const pop = useCallback(() => {
    setRoutes((r) => {
      const clone = [...r];
      clone.pop();
      return clone;
    });
  }, []);
  const close = useCallback(() => {
    // setRoutes([]);
    setVisible(false);
  }, []);
  const clear = useCallback(() => {
    setRoutes([]);
  }, []);
  return [{
    visible,
    routes,
    match,
    open,
    push,
    pop,
    close,
    clear,
    eventsMap: eventsMap.current,
    // setVisible,
  }];
};
interface DetailContainerProps {
  visible: boolean
  routes: IRoute[]
  match: IRoute
  open: (route: IRoute) => void
  push: (nextRoute: IRoute) => void
  pop: () => void
  close: () => void
  clear: () => void
  eventsMap: Map<string, DetailEvents>
  // setVisible: (visible: boolean) => void
}
const DetailContainer: React.FC<DetailContainerProps> = ({ children, visible, ...props }) => (
  <DetailContainerContext.Provider value={{
    topAnnouncementHeight: HeaderStore.announcementClosed ? 0 : 50,
    outside: false,
    ...props,
  }}
  >
    <Animate
      component="div"
      transitionAppear
      transitionName="slide-right"
      onLeave={props.clear}
    >
      {visible ? (
        <Container>{children}</Container>
      ) : null}
    </Animate>
  </DetailContainerContext.Provider>
);

export default DetailContainer;
