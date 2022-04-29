import isEqual from 'lodash/isEqual';
import React, { useRef } from 'react';
/**
 * 深度比较deps的优化useCreation
 * 避免memo值 重新计算
 * @param factory
 * @param deps
 * @returns
 */
function useDeepCompareCreation<T>(factory: () => T, deps: React.DependencyList): T {
  const { current } = useRef({ obj: undefined as T | undefined, deps, initialized: false });
  if (current.initialized === false || !isEqual(current.deps, deps)) {
    current.deps = deps;
    current.obj = factory();
    current.initialized = true;
  }
  return current.obj!;
}

export default useDeepCompareCreation;
