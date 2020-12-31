import React, { useCallback, useState, useEffect } from 'react';
import { Animate } from 'choerodon-ui';
import { stores } from '@choerodon/boot';
import Container from './Container';
import DetailContainerContext, { IRoute } from './context';
// 默认展示children，push之后再匹配
const { HeaderStore } = stores;
export const useDetail = (): [DetailContainerProps] => {
  const [routes, setRoutes] = useState<IRoute[]>([]);
  const [visible, setVisible] = useState(false);
  const match = routes[routes.length - 1];
  const push = useCallback((nextRoute: IRoute) => {
    setRoutes((r) => ([...r, nextRoute]));
    setVisible(true);
  }, []);
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
    push,
    pop,
    close,
    clear,
    // setVisible,
  }];
};
interface DetailContainerProps {
  visible: boolean
  routes: IRoute[]
  match: IRoute
  push: (nextRoute: IRoute) => void
  pop: () => void
  close: () => void
  clear: () => void
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
