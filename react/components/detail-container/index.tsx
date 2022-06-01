import React, {
  useCallback, useState, useRef,
} from 'react';

import { Animate } from 'choerodon-ui/pro';
import { useCreation, usePersistFn } from 'ahooks';
import { observer } from 'mobx-react-lite';
import { merge } from 'lodash';
import useGetAnnouncementHeight from '@/hooks/useGetAnnouncementHeight';
import Container, { registerPath } from './Container';
import PreviewIssueFile from './PreviewIssueFile';
import DetailContainerContext, {
  IDetailPushRouteOptions, IPreview, IRoute, IRouteWithKey,
} from './context';
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
  const currentDetailRouteOptions = useCreation(() => ({}) as IDetailPushRouteOptions, []);
  const eventsMap = useRef<Map<string, DetailEvents>>(new Map());
  const updateEventsMap = useCallback((path: string, events?: DetailEvents) => {
    if (events) {
      eventsMap.current.set(path, events);
    }
  }, []);
  const match = routes[routes.length - 1];
  const push = usePersistFn((nextRoute: IRoute) => {
    const pushDetail = () => {
      const routeWithKey = { ...merge(nextRoute, currentDetailRouteOptions[nextRoute.path]), key: Math.random() };
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

  const open = usePersistFn((route: IRoute, pushRouteOptions?: IDetailPushRouteOptions) => {
    const openDetail = () => {
      const routeWithKey = { ...route, key: Math.random() };
      setRoutes([routeWithKey]);
      updateEventsMap(routeWithKey.path, routeWithKey.events);
      setVisible(true);
      setDescriptionChanged(false);
      setCopingStrategyChanged(false);
    };
    pushRouteOptions && Object.assign(currentDetailRouteOptions, pushRouteOptions);
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
  /**
   * 打开一个新的详情侧边栏（会清空当前详情路由信息）
  * @param route 当前打开的路由
  * @param pushRouteOptions 在当前路由进行push操作时附加的属性
  */
  open: (route: IRoute, pushRouteOptions?: IDetailPushRouteOptions) => void
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
  disableResizeWidth?: boolean
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
