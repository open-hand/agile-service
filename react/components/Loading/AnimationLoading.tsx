import React, {
  useState, useCallback,
  useMemo,
  useContext,
  useEffect,
  useImperativeHandle,
  useRef,
} from 'react';
import { AnimationLoading as OriginAnimationLoading } from '@choerodon/components';
import classNames from 'classnames';
import './AnimationLoading.less';

import {
  noop, omit, uniqueId, merge,
} from 'lodash';
import {
  useMap, useMount, usePersistFn, useUpdateEffect, useUnmount, useCreation, useSafeState,
} from 'ahooks';

const prefixCls = 'c7n-agile-animation-loading';

interface ILoadingProps {
  loadId?: string /** 加载唯一id */
  loading?: boolean
  // loadedUnmount?: boolean /** @default 'true' 加载完成后是否卸载加载的loading */
  noDeliverLoading?: boolean /**  不去向父级传递loading @default 'false' */
  allowSelfLoading?: boolean /** 允许在父级Loading结束后调用自身的loading @default 'false'' */
  className?: string
  style?: React.CSSProperties
  // contentClassName?: string
  // contentStyle?: React.CSSProperties
}
interface ILoadingProviderProps {
  loadId?: string /** 全局加载Loading 唯一id  默认为 parent-provider */
  loading?: boolean /** 全局loading状态 */
  globalSingle?: boolean /** 启用全局单loading  @default 'true' 未对此配置进行处理 */
  ref?: React.Ref<ContextProps>
}
interface ILoadingRegisterChildrenData extends Pick<ILoadingProps, 'allowSelfLoading'> {
  loadId: string
  changeLoading: React.Dispatch<React.SetStateAction<boolean | undefined>>
}
interface ILoadingChangeExtraConfig extends Pick<ILoadingProps, 'allowSelfLoading'> {

}
interface ContextProps {
  loading: boolean
  change: (key: string, loading: boolean, extraConfig?: ILoadingChangeExtraConfig) => void /** 改变loading  父级Loading每次变更为true时会关闭所有子Loading 并重置 */
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
/**
 * 父级Loading
 */
export const LoadingProvider: React.FC<ILoadingProviderProps> = (props) => {
  const [loadMaps, { set: setMap, remove: removeMap, setAll }] = useMap<string, { status: 'init' | 'ready' | 'doing' } & ILoadingRegisterChildrenData>();
  const [globalLoading, setGlobalLoading] = useSafeState(false);
  const globalLoadingTrigger = useRef<string>(); // 记录触发Loading层级
  const globalLoadId = useCreation(() => props.loadId || 'parent-provider', []);
  const handleChangeGlobal = useCallback((newLoading: boolean) => {
    setGlobalLoading(() => {
      console.log('loading....setGlobalLoading', newLoading);
      if (newLoading) {
        // 关闭子Loading
        setAll([...loadMaps.entries()].map(([key, value]) => {
          value.changeLoading(false);
          return ([key, { ...value, status: 'init' }]);
        }));
        globalLoadingTrigger.current = globalLoadId;
        return true;
      }
      globalLoadingTrigger.current = undefined;
      return false;
    });
  }, []);
  const handleChange = useCallback((loadId: string, newLoading: boolean, extraConfig?: ILoadingChangeExtraConfig) => {
    console.log('----------------------start-----------------------------------');
    // 全局 Loading 状态更改调用全局方法
    if (loadId === globalLoadId) {
      handleChangeGlobal(newLoading);
      console.log('----------------------end-----------------------------------');

      return;
    }
    const childrenLoad = loadMaps.get(loadId);
    console.log(childrenLoad, 'start..handleChange', loadId, newLoading, extraConfig, loadMaps);

    if (childrenLoad) {
      // if (props.globalSingle) {
      //   childrenLoad.changeLoading(newLoading);
      //   return;
      // }
      const newStatus = newLoading ? 'doing' : 'ready';
      const newExtraConfig = merge(omit(childrenLoad, 'status'), extraConfig);
      console.log(`change [${loadId}] status:${loadMaps.get(loadId)?.status}-->loading:[${newLoading}] lastLoadingTrigger:${globalLoadingTrigger.current}  `, newExtraConfig.allowSelfLoading);

      loadMaps.set(loadId, { ...newExtraConfig, status: newStatus });
      // 当初始化完成后 有设置独立loading时  判断是否有正在加载的全局loading 有则跳过，否则
      if (newExtraConfig.allowSelfLoading && !globalLoadingTrigger.current) {
        console.log(`【self】 ${loadId} change loading ${newLoading}`);
        childrenLoad?.changeLoading(newLoading);
        console.log('----------------------end-----------------------------------');

        return;
      }

      const newGlobalLoading = [...loadMaps.values()].some((i) => i.status === 'doing');
      globalLoadingTrigger.current = newGlobalLoading ? loadId : undefined;
      console.log(`setGlobalLoading:${newGlobalLoading}`, [...loadMaps.values()]);
      setGlobalLoading(() => newGlobalLoading);
      console.log('----------------------end-----------------------------------');

      // setLoading(globalLoading);
    }
  }, [globalLoadId, handleChangeGlobal, loadMaps, setGlobalLoading]);
  const handleRegister = usePersistFn((data: ILoadingRegisterChildrenData) => {
    setMap(data.loadId, { status: 'init', ...data });
  });
  const handleCancelResigner = usePersistFn((loadId: string) => {
    removeMap(loadId);
  });
  useEffect(() => {
    if (props.loading !== undefined) {
      handleChangeGlobal(props.loading);
    }
  }, [handleChangeGlobal, props.loading]);

  useImperativeHandle(props.ref, () => ({
    loading: globalLoading,
    isHasProvider: true,
    registerChildren: handleRegister,
    cancelRegisterChildren: handleCancelResigner,
    change: handleChange,
  }));
  return (
    <Context.Provider value={{
      loading: globalLoading,
      isHasProvider: true,
      registerChildren: handleRegister,
      cancelRegisterChildren: handleCancelResigner,
      change: handleChange,
    }}
    >
      <Loading loading={globalLoading} loadId={globalLoadId} className={`${prefixCls}-global`} noDeliverLoading style={{ display: globalLoading ? 'unset' : 'none' }} />
      {props.children}
    </Context.Provider>
  );
};
/**
 * 动画Loading
 */
const AnimationLoading: React.FC<Pick<ILoadingProps, 'loading' | 'className' | 'style'>> = ({
  loading, className, children, style,
}) => (
  <div
    className={classNames(prefixCls, {
      [`${prefixCls}-no-children`]: !children,
      [`${prefixCls}-no-children-hidden`]: !children && !loading,
    }, className)}
    style={style}
  >
    <OriginAnimationLoading display={loading} className={classNames(`${prefixCls}-container`, { [`${prefixCls}-hidden`]: !loading })} />
    {children}
  </div>
);
/**
 * 子Loading
 */
const Loading: React.FC<ILoadingProps> = ({
  children, loading: propsLoading, noDeliverLoading, loadId: propsLoadId, allowSelfLoading, ...otherProps
}) => {
  const [loading, setLoading] = useState<boolean | undefined>(() => propsLoading); /** 自身的loading */
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
      console.log('useUpdateEffect', propsLoading);
      isHasProvider && !noDeliverLoading ? change(loadId, !!propsLoading) : setLoading(propsLoading);
    }
  }, [isHasProvider, loadId, propsLoading]);

  return (
    <AnimationLoading loading={!!loading} {...otherProps}>
      {children}
    </AnimationLoading>
  );
};
/**
 * loading时隐藏节点
 */
export const LoadingHiddenWrap: React.FC = ({ children }) => {
  const { loading } = useLoading();
  if (loading) {
    return (
      <span style={{ visibility: 'hidden' }}>
        {children}
      </span>
    );
  }
  return children as any;
};

export default Loading;
