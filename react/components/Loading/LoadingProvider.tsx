import React, {
  useCallback, useContext, useEffect, useImperativeHandle,
  useMemo, useState,
} from 'react';
import {
  noop, merge, cloneDeep,
} from 'lodash';
import {
  useMap, usePersistFn, useUnmount, useCreation, useSafeState,
} from 'ahooks';
import Loading from './LoadingChildren';
import {
  ILoadingRegisterChildrenData, ILoadingChangeExtraConfig, ILoadingChangeItem, ILoadingChildren, ILoadingChangeStatus,
} from './type';
import { filterSelfLoading } from './utils';

interface ILoadingProviderProps {
  loadId?: string /** 全局加载Loading 唯一id  默认为 parent-provider */
  loading?: boolean /** 全局loading状态 */
  globalSingle?: boolean /** 启用全局单loading  @default 'true' 未对此配置进行处理 */
  ref?: React.Ref<ContextProps>
}
interface ContextProps {
  loading: boolean
  childrenLoadMap: Map<string, ILoadingChildren>
  change: (key: string, loading: boolean, extraConfig?: ILoadingChangeExtraConfig) => void /** 改变loading  父级Loading每次变更为true时会关闭所有子Loading 并重置 */
  registerChildren: (data: ILoadingRegisterChildrenData) => void /** 注册子loading */
  cancelRegisterChildren: (loadId: string) => void /**  取消注册子loading */
  isHasProvider: boolean /**  标记是否有父级Provider */

}
const Context = React.createContext({
  loading: false, isHasProvider: false, cancelRegisterChildren: noop, registerChildren: noop, change: noop, childrenLoadMap: new Map(),
} as ContextProps);
export function useLoading() {
  return useContext(Context);
}
export { Context as LoadingContext };
/**
 * 父级Loading 提供者，管理内部所有的子Loading 目前全局Loading使用的是吐泡泡Loading
 * 初始化时，所有子Loading状态会直接传递给父级
 * 当直接操作父级Loading时，所有子Loading会自动关闭并重新注册
 *
 */
const LoadingProvider: React.FC<ILoadingProviderProps> = (props) => {
  // 正在进行的子Loading
  const [loadMaps, {
    set: setMap, remove: removeMap, setAll, reset,
  }] = useMap<string, ILoadingChangeItem>();
  const [globalLoading, setGlobalLoading] = useSafeState(false);
  // con3st
  const [childrenLoadingStatus, setChildrenLoadingStatus] = useState<Map<string, ILoadingChildren>>(new Map());
  const childrenLoadMap = useCreation(() => new Map<string, ILoadingChildren>(), []);// 最新数据
  const globalLoadId = useCreation(() => props.loadId || 'parent-provider', []);
  const handleChangeGlobal = useCallback((newLoading: boolean) => {
    setGlobalLoading(() => {
      console.log('loading....setGlobalLoading', newLoading);
      if (newLoading) {
        // 重置所有子Loading
        childrenLoadMap.forEach((value) => {
          value.changeLoading(false);
          childrenLoadMap.set(value.loadId, { ...value, status: 'init', finishInit: false });
        });
        return true;
      }
      return false;
    });
  }, [childrenLoadMap, setGlobalLoading]);

  const handleChange = useCallback((loadId: string, newLoading: boolean, extraConfig?: ILoadingChangeExtraConfig) => {
    console.log('----------------------start-----------------------');
    // 全局 Loading 状态更改调用全局方法
    if (loadId === globalLoadId) {
      handleChangeGlobal(newLoading);
      return;
    }
    // 增加到状态更新Map中
    const newStatus = newLoading ? 'doing' : 'ready';
    setMap(loadId, { ...extraConfig, status: newStatus, loadId });
  }, [globalLoadId, handleChangeGlobal, setMap]);

  // 响应更新maps
  useEffect(() => {
    if (loadMaps.size > 0 && childrenLoadMap.size > 0) {
      const statusStack = [...loadMaps.values()];
      console.log('....statusStack', cloneDeep(statusStack));
      // 不是初始状态的子Loading 并且有配置允许使用自己的Loading 即可自更新
      const selfLoadingStatus = statusStack.filter((i) => filterSelfLoading(i, childrenLoadMap.get(i.loadId)));

      const selfLoadingStatusIds = selfLoadingStatus.map((i) => i.loadId);
      const globalDoingStatus = statusStack.filter((i) => i.status === 'doing').filter((i) => !selfLoadingStatusIds.includes(i.loadId));
      console.log('self,children,globalDoing', selfLoadingStatus, cloneDeep([...childrenLoadMap.values()]), globalDoingStatus);
      const newChildrenLoadStatus = [] as any[];
      // 进行子Loading属性配置更新
      statusStack.forEach((item) => {
        // 未注册的子Loading不进行属性更新
        if (childrenLoadMap.has(item.loadId)) {
          const childrenLoadValue = merge(childrenLoadMap.get(item.loadId), item);
          childrenLoadMap.set(item.loadId, childrenLoadValue);
          newChildrenLoadStatus.push({ loadId: item.loadId, status: childrenLoadValue.status });
        }
      });
      setChildrenLoadingStatus(childrenLoadMap);
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
    }
  }, [loadMaps, setGlobalLoading, childrenLoadMap.size, childrenLoadMap]);
  const handleRegister = usePersistFn((data: ILoadingRegisterChildrenData) => {
    childrenLoadMap.set(data.loadId, { ...data, status: 'init', initStatus: data.initStatus || 'init' });
  });
  const handleCancelResigner = usePersistFn((loadId: string) => {
    childrenLoadMap.delete(loadId);
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
    childrenLoadMap: childrenLoadingStatus,
    registerChildren: handleRegister,
    cancelRegisterChildren: handleCancelResigner,
    change: handleChange,
  }));
  return (
    <Context.Provider value={{
      loading: globalLoading,
      isHasProvider: true,
      childrenLoadMap,
      registerChildren: handleRegister,
      cancelRegisterChildren: handleCancelResigner,
      change: handleChange,
    }}
    >
      <Loading loading={globalLoading} loadId={globalLoadId} noDeliverLoading globalLoading style={{ display: globalLoading ? 'unset' : 'none' }} />
      {props.children}
    </Context.Provider>
  );
};

/**
 * loading进行中隐藏节点
 */
export const LoadingHiddenWrap: React.FC<{ loadIds?: string[] }> = ({ children, loadIds }) => {
  const { loading, childrenLoadMap } = useLoading();
  const isHidden = useMemo(() => {
    if (loading) {
      return true;
    }
    if (loadIds?.length) {
      console.log('.....', loadIds.map((i) => childrenLoadMap.get(i)?.status));
      return !!loadIds.map((i) => childrenLoadMap.get(i)?.status === 'doing').filter(Boolean).length;
    }
    return false;
  }, [childrenLoadMap, loadIds, loading]);
  if (isHidden) {
    return (
      <span style={{ visibility: 'hidden' }}>
        {children}
      </span>
    );
  }
  return children as any;
};

export default LoadingProvider;
