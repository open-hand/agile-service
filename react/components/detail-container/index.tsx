import React, { useCallback, useState } from 'react';
import { Animate } from 'choerodon-ui';
import { stores } from '@choerodon/boot';
import Container from './Container';
import DetailContainerContext, { IRoute } from './context';
// 默认展示children，push之后再匹配
const { HeaderStore } = stores;
const DetailContainer: React.FC = ({ children }) => {
  const [routes, setRoutes] = useState<IRoute[]>(['issue']);
  const [visible, setVisible] = useState(false);
  // const visible = routes.length > 0;
  const match = routes[routes.length - 1];
  const push = useCallback((nextRoute: IRoute) => {
    setRoutes((r) => ([...r, nextRoute]));
  }, []);
  const pop = useCallback(() => {
    setRoutes((r) => {
      const clone = [...r];
      clone.pop();
      return clone;
    });
  }, []);
  const close = useCallback(() => {
    setRoutes([]);
  }, []);
  return (
    <DetailContainerContext.Provider value={{
      topAnnouncementHeight: HeaderStore.announcementClosed ? 0 : 50,
      outside: false,
      routes,
      match,
      push,
      pop,
      close,
      setVisible,
    }}
    >
      <Animate
        component="div"
        transitionAppear
        transitionName="slide-right"
        onLeave={() => {
          // 侧边完全关闭后，清除数据
          // store.destroy();
        }}
      >
        {visible ? (
          <Container>{children}</Container>
        ) : null}
      </Animate>
    </DetailContainerContext.Provider>
  );
};

export default DetailContainer;
