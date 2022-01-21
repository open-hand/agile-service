import React, { useEffect, useRef } from 'react';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';

function useUnmountSaveCache<T>(code: string, data: T extends Function ? () => T : T) {
  const dataRef = useRef<any>();
  dataRef.current = data;
  useEffect(() => {
    const projectId = String(localPageCacheStore.projectId);
    return () => {
      const d = typeof dataRef.current === 'function' ? dataRef.current() : dataRef.current;
      localPageCacheStore.project(projectId).setItem(code, d);
    };
  }, [code]);
}
export default useUnmountSaveCache;
