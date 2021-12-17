import React, { useEffect } from 'react';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';

import { CacheStoreInterface } from '@/stores/common/CacheBaseStore';

function useUnmountSaveCache(code: string, data: () => any | any) {
  useEffect(() => {
    const projectId = String(localPageCacheStore.projectId);
    return () => {
      const d = typeof data === 'function' ? data() : data;
      localPageCacheStore.project(projectId).setItem(code, d);
    };
  }, [code, data]);
}
export default useUnmountSaveCache;
