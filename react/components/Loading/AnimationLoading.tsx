import React, {
  useEffect, useState, useCallback,
  useMemo,
  useContext,
  useRef,
  useReducer,
} from 'react';
import { AnimationLoading as OriginAnimationLoading } from '@choerodon/components';
import classNames from 'classnames';
import './AnimationLoading.less';

import { noop, uniqueId } from 'lodash';
import {
  useMap, useMount, usePersistFn, useSet, useUpdateEffect, useWhyDidYouUpdate, useUnmount, useUpdate, usePrevious, useSafeState, useCreation, useReactive,
} from 'ahooks';
import { useObservable } from 'mobx-react-lite';

const prefixCls = 'c7n-agile-animation-loading';

interface ILoadingProps {
  loadId?: string /** 加载唯一id */
  loading?: boolean
  loadedUnmount?: boolean /** @default 'true' 加载完成后是否卸载加载的loading */
  noDeliverLoading?: boolean /**  不去向父级传递loading @default 'false' */
  allowSelfLoading?: boolean /** 允许调用自身的loading @default 'false'' */
  className?: string
  style?: React.CSSProperties
  contentClassName?: string
  contentStyle?: React.CSSProperties
}
interface ILoadingProviderProps {
  defaultLoading?: boolean /** 默认loading状态 */
  loading?: boolean /** 全局loading状态 */
  globalSingle?: boolean /** 启用全局单loading */
}
interface ILoadingRegisterChildrenData extends Pick<ILoadingProps, 'allowSelfLoading'> {
  loadId: string
  changeLoading: React.Dispatch<React.SetStateAction<boolean | undefined>>
}
interface ILoadingChangeExtraConfig extends Pick<ILoadingProps, 'allowSelfLoading'> {

}
interface ContextProps {
  loading: boolean
  change: (key: string, loading: boolean, extraConfig?: ILoadingChangeExtraConfig) => void /** 改变loading  */
  registerChildren: (data: ILoadingRegisterChildrenData) => void /** 注册子loading */
  cancelRegisterChildren: (loadId: string) => void /**  取消注册子loading */
  isHasProvider: boolean /**  标记是否有父级Provider */

}
const Context = React.createContext({
  loading: false, isHasProvider: false, cancelRegisterChildren: noop, registerChildren: noop, change: noop,
} as ContextProps);
export function useLoading() {
  return useContext(Context);
}
export const LoadingProvider: React.FC<ILoadingProviderProps> = (props) => {
  // useObservable(new Map<string,{ status: 'init' | 'ready' | 'doing' } & ILoadingRegisterChildrenData>() );
  const [loadMaps, { set: setMap, remove: removeMap }] = useMap<string, { status: 'init' | 'ready' | 'doing' } & ILoadingRegisterChildrenData>();
  const [loading, setLoading] = useState(false);
  const [taskStack, setStack] = useState<Array<{ loadId: string, status: 'init' | 'ready' | 'doing' }>>([]);
  const handleChange = useCallback((loadId: string, newLoading: boolean, extraConfig?: ILoadingChangeExtraConfig) => {
    const childrenLoad = loadMaps.get(loadId);
    const { allowSelfLoading = childrenLoad?.allowSelfLoading } = extraConfig || {};

    if (childrenLoad) {
      // 当初始化完成后 有设置独立loading时  判断是否有正在加载的全局loading 有则跳过，否则
      if (allowSelfLoading && ![...loadMaps.values()].some((i) => ['doing', 'init'].includes(i.status))) {
        console.log(`【self】 ${loadId} change loading ${newLoading}`);
        childrenLoad?.changeLoading((old) => (old !== newLoading ? newLoading : old));
        return;
      }
      console.log(`change [${loadId}] status:${loadMaps.get(loadId)?.status}-->loading:[${newLoading}] `, allowSelfLoading);

      loadMaps.set(loadId, { ...childrenLoad, status: newLoading ? 'doing' : 'ready', ...extraConfig });
      const globalLoading = [...loadMaps.values()].some((i) => i.status === 'doing');
      console.log('loadMaps:', [...loadMaps.values()]);
      console.log(`globalLoading:${globalLoading}`);
      setLoading(() => globalLoading);
      // setLoading(globalLoading);
    }
  }, [loadMaps]);
  const handleRegister = usePersistFn((data: ILoadingRegisterChildrenData) => {
    setMap(data.loadId, { status: 'init', ...data });
  });
  const handleCancelResigner = usePersistFn((loadId: string) => {
    removeMap(loadId);
  });

  return (
    <Context.Provider value={{
      loading: false,
      isHasProvider: true,
      registerChildren: handleRegister,
      cancelRegisterChildren: handleCancelResigner,
      change: handleChange,
    }}
    >
      <AnimationLoading loading={loading}>
        <span />
      </AnimationLoading>
      {props.children}
    </Context.Provider>
  );
};

const AnimationLoading: React.FC<Pick<ILoadingProps, 'loading' | 'className' | 'loadedUnmount' | 'contentStyle' | 'contentClassName'>> = ({
  loading,
  loadedUnmount = true, className, contentStyle, contentClassName, children,
}) => (!loadedUnmount || loading ? (
  <div className={classNames(prefixCls, className)}>
    <div className={classNames(`${prefixCls}-container`, { [`${prefixCls}-container-loading`]: loading })}>
      <OriginAnimationLoading display={loading} />
    </div>
    <div style={{ visibility: loading ? 'hidden' : 'visible', ...contentStyle }} className={contentClassName}>
      {children}
    </div>
  </div>
) : <>{children}</>);

const Loading: React.FC<ILoadingProps> = ({
  children, loading: propsLoading, noDeliverLoading, loadId: propsLoadId, allowSelfLoading, ...otherProps
}) => {
  const [loading, setLoading] = useState<boolean>(); /** 自身的loading */
  const {
    registerChildren, cancelRegisterChildren, isHasProvider, change,
  } = useContext(Context);

  const loadId = useMemo(() => propsLoadId || uniqueId('loading'), [propsLoadId]);
  useMount(() => {
    !noDeliverLoading && registerChildren({ loadId, changeLoading: setLoading, allowSelfLoading });
  });
  useUnmount(() => {
    cancelRegisterChildren(loadId);
  });

  useUpdateEffect(() => {
    // 兼容过往使用loading方法的 effect
    if (propsLoading !== undefined) {
      isHasProvider ? change(loadId, !!propsLoading) : setLoading(propsLoading);
    }
  }, [change, isHasProvider, loadId, propsLoading]);
  useWhyDidYouUpdate(`${loadId}`, {
    change, isHasProvider, loadId, propsLoading,
  });
  console.log('loading', loadId, loading);
  return (
    <AnimationLoading loading={!!loading} {...otherProps} loadedUnmount={!!children}>
      {children}
    </AnimationLoading>
  );
};

export default Loading;
