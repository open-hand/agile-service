import React, {
  useCallback, useState, useRef,
} from 'react';

import { Animate } from 'choerodon-ui/pro';
import { usePersistFn } from 'ahooks';
import { observer } from 'mobx-react-lite';
import useGetAnnouncementHeight from '@/hooks/useGetAnnouncementHeight';
import Container, { registerPath } from './Container';
import PreviewIssueFile from './PreviewIssueFile';
import DetailContainerContext, { IPreview, IRoute, IRouteWithKey } from './context';
import openDescriptionConfirm from './openDescriptionConfirm';
// 默认展示children，push之后再匹配
export { registerPath };
export interface DetailEvents {
  [type: string]: (...args: any[]) => void
}
export const useDetail = (): [DetailContainerProps] => {
  const [routes, setRoutes] = useState<IRouteWithKey[]>([]);
  const [visible, setVisible] = useState(false);
  const [filePreview, setFilePreview] = useState<IPreview>();
  const [hidden, setHidden] = useState(true);
  const [descriptionChanged, setDescriptionChanged] = useState<boolean>(false);
  const [copingStrategyChanged, setCopingStrategyChanged] = useState<boolean>(false);
  const eventsMap = useRef<Map<string, DetailEvents>>(new Map());
  const updateEventsMap = useCallback((path: string, events?: DetailEvents) => {
    if (events) {
      eventsMap.current.set(path, events);
    }
  }, []);
  const match = routes[routes.length - 1];
  const push = usePersistFn((nextRoute: IRoute) => {
    const pushDetail = () => {
      const routeWithKey = { ...nextRoute, key: Math.random() };
      setRoutes((r) => ([...r, routeWithKey]));
      updateEventsMap(routeWithKey.path, routeWithKey.events);
      setVisible(true);
      setDescriptionChanged(false);
      setCopingStrategyChanged(false);
    };
    if (!descriptionChanged && !copingStrategyChanged) {
      pushDetail();
    } else {
      openDescriptionConfirm({
        onOk: pushDetail,
        type: copingStrategyChanged ? '应对策略' : '',
      });
    }
  });
  const open = usePersistFn((route: IRoute) => {
    const openDetail = () => {
      const routeWithKey = { ...route, key: Math.random() };
      setRoutes([routeWithKey]);
      updateEventsMap(routeWithKey.path, routeWithKey.events);
      setVisible(true);
      setDescriptionChanged(false);
      setCopingStrategyChanged(false);
    };
    if (!descriptionChanged && !copingStrategyChanged) {
      openDetail();
    } else {
      openDescriptionConfirm({
        onOk: openDetail,
        type: copingStrategyChanged ? '应对策略' : '',
      });
    }
  });
  const pop = usePersistFn(() => {
    if (!descriptionChanged && !copingStrategyChanged) {
      setRoutes((r) => {
        const clone = [...r];
        clone.pop();
        return clone;
      });
    } else {
      openDescriptionConfirm({
        onOk: () => {
          setRoutes((r) => {
            const clone = [...r];
            clone.pop();
            return clone;
          });
          setDescriptionChanged(false);
          setCopingStrategyChanged(false);
        },
        type: copingStrategyChanged ? '应对策略' : '',
      });
    }
  });
  const close = usePersistFn(() => {
    // setRoutes([]);
    if (!descriptionChanged && !copingStrategyChanged) {
      if (filePreview) {
        setHidden(true);
      } else {
        setVisible(false);
      }
    } else {
      openDescriptionConfirm({
        onOk: () => {
          if (filePreview) {
            setHidden(true);
          } else {
            setVisible(false);
          }
          setDescriptionChanged(false);
          setCopingStrategyChanged(false);
        },
        type: copingStrategyChanged ? '应对策略' : '',
      });
    }
  });

  const clear = useCallback(() => {
    eventsMap.current.forEach((events) => {
      if (events.close) {
        events.close();
      }
    });
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
    filePreview,
    setFilePreview,
    hidden,
    setHidden,
    descriptionChanged,
    setDescriptionChanged,
    copingStrategyChanged,
    setCopingStrategyChanged,
    // setVisible,
  }];
};
export interface DetailContainerProps {
  descriptionChanged: boolean
  setDescriptionChanged: (changed: boolean) => void
  copingStrategyChanged: boolean
  setCopingStrategyChanged: (changed: boolean) => void
  visible: boolean
  routes: IRouteWithKey[]
  match: IRouteWithKey
  open: (route: IRoute) => void
  push: (nextRoute: IRoute) => void
  pop: () => void
  close: () => void
  clear: () => void
  eventsMap: Map<string, DetailEvents>
  fullPage?: boolean
  filePreview?: IPreview
  setFilePreview: (filePreview?: IPreview) => void
  hidden: boolean
  setHidden: (hidden: boolean) => void
}
const DetailContainer: React.FC<DetailContainerProps> = ({ children, visible, ...props }) => {
  const resizeRef = useRef();
  const element = visible ? (
    <Container>{children}</Container>
  ) : null;
  const topAnnouncementHeight = useGetAnnouncementHeight();
  return (
    <DetailContainerContext.Provider value={{
      topAnnouncementHeight,
      outside: false,
      resizeRef,
      ...props,
    }}
    >
      <>
        <PreviewIssueFile />
        {props.fullPage ? element : (
          <Animate
            component="div"
            transitionAppear
            transitionName="slide-right"
            onLeave={props.clear}
          >
            {element}
          </Animate>
        )}
      </>
    </DetailContainerContext.Provider>
  );
};

export default observer(DetailContainer);
