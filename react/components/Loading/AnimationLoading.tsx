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
  noop, uniqueId, merge, cloneDeep,
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
type ILoadingChangeStatus = 'init' | 'ready' | 'doing'
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
 * 初始化时，所有子Loading状态会直接传递给父级
 * 当直接操作父级Loading时，所有子Loading会自动关闭并重新注册
 *
 */
export const LoadingProvider: React.FC<ILoadingProviderProps> = (props) => {
  // 正在进行的子Loading
  const [loadMaps, {
    set: setMap, remove: removeMap, setAll, reset,
  }] = useMap<string, { status: ILoadingChangeStatus, loadId: string } & ILoadingChangeExtraConfig>();
  const [globalLoading, setGlobalLoading] = useSafeState(false);
  // const
  const childrenLoadMap = useCreation(() => new Map<string, { status: ILoadingChangeStatus } & ILoadingRegisterChildrenData>(), []);// 最新数据
  const globalLoadingTrigger = useRef<string>(); // 记录触发Loading层级
  const globalLoadId = useCreation(() => props.loadId || 'parent-provider', []);
  const handleChangeGlobal = useCallback((newLoading: boolean) => {
    setGlobalLoading(() => {
      console.log('loading....setGlobalLoading', newLoading);
      if (newLoading) {
        // 重置所有子Loading
        childrenLoadMap.forEach((value) => {
          value.changeLoading(false);
          childrenLoadMap.set(value.loadId, { ...value, status: 'init' });
        });
        globalLoadingTrigger.current = globalLoadId;
        return true;
      }
      globalLoadingTrigger.current = undefined;
      return false;
    });
  }, []);

  const handleChange = useCallback((loadId: string, newLoading: boolean, extraConfig?: ILoadingChangeExtraConfig) => {
    console.log('----------------------start-----------------------');
    // 全局 Loading 状态更改调用全局方法
    if (loadId === globalLoadId) {
      handleChangeGlobal(newLoading);
      console.log('----------------------end-----------------------------------');
      return;
    }
    // 增加到状态更新队列中
    const newStatus = newLoading ? 'doing' : 'ready';
    setMap(loadId, { ...extraConfig, status: newStatus, loadId });
    // statusStack.push({ ...extraConfig, status: newStatus, loadId });
    console.log('childrenLoad', loadId, newLoading);
  }, [globalLoadId, handleChangeGlobal, setMap]);
  // 更新全局
  useEffect(() => {
    if (loadMaps.size > 0 && childrenLoadMap.size > 0) {
      const statusStack = [...loadMaps.values()];
      console.log('star....statusStack', cloneDeep(statusStack));
      // 不是初始状态的子Loading 并且有配置允许使用自己的Loading 即可自更新
      const selfLoadingStatus = statusStack.filter((i) => childrenLoadMap.get(i.loadId)?.status !== 'init' && (childrenLoadMap.get(i.loadId)?.allowSelfLoading || i.allowSelfLoading));

      const selfLoadingStatusIds = selfLoadingStatus.map((i) => i.loadId);
      const globalDoingStatus = statusStack.filter((i) => i.status === 'doing').filter((i) => !selfLoadingStatusIds.includes(i.loadId));
      console.log('useUpdateEffect', selfLoadingStatus, [...childrenLoadMap.values()], globalDoingStatus);

      // 进行子Loading属性配置更新
      statusStack.forEach((item) => {
        // 未注册的子Loading不进行属性更新
        if (childrenLoadMap.has(item.loadId)) {
          const childrenLoadValue = merge(childrenLoadMap.get(item.loadId), item);
          childrenLoadMap.set(item.loadId, childrenLoadValue);
        }
      });
      // 如果有全局更新   则不允许其余子Loading出现
      if (globalDoingStatus.length > 0) {
        setGlobalLoading(true);
        childrenLoadMap.forEach((children) => {
          children.changeLoading(false);
        });
        return;
      }
      setGlobalLoading(false);
      // 子Loading 进行状态更新
      selfLoadingStatus.forEach((item) => {
        console.log('self..', item.loadId, item.status === 'doing');
        childrenLoadMap.get(item.loadId)?.changeLoading(item.status === 'doing');
      });
      console.log('finish Update');
      // statusStack.length = 0;
      // setStatusStack(() => []);
    }
  }, [loadMaps, setGlobalLoading, childrenLoadMap.size, childrenLoadMap]);
  const handleRegister = usePersistFn((data: ILoadingRegisterChildrenData) => {
    childrenLoadMap.set(data.loadId, { ...data, status: 'init' });
  });
  const handleCancelResigner = usePersistFn((loadId: string) => {
    childrenLoadMap.delete(loadId);
    // removeMap(loadId);
  });
  useEffect(() => {
    if (props.loading !== undefined) {
      handleChangeGlobal(props.loading);
    }
  }, [handleChangeGlobal, props.loading]);
  useUnmount(() => {
    reset();
    childrenLoadMap.clear();
  });
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
  const [loading, setLoading] = useState<boolean | undefined>(); /** 自身的loading */
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
      console.log(`Loading:${loadId} USE:${isHasProvider && !noDeliverLoading ? 'change' : 'setLoading'}`, propsLoading);
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
