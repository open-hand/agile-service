import { getProjectId } from '@/utils/common';
import { merge } from 'lodash';

interface LocalPageCacheStoreInterface {
  getItem: (code: string) => any,
  setItem: (code: string, data: any) => void,
  remove: (code: string) => void,
  clear: () => void,
}
const pages = new Map<string, any>();
const currentProjectId = getProjectId();
class LocalPageCacheStore implements LocalPageCacheStoreInterface {
  pageKeyList = ['scrumboard', 'issues']

  setItem(pageKey: string, data: any) {
    pages.set(`${currentProjectId}-${pageKey}`, data);
  }

  mergeSetItem<T extends object>(pageKey: string, data: T) {
    pages.set(`${currentProjectId}-${pageKey}`, merge(pages.get(pageKey), data));
  }

  getItem(pageKey: string) {
    return pages.get(`${currentProjectId}-${pageKey}`);
  }

  remove(pageKey: string) {
    pages.delete(`${currentProjectId}-${pageKey}`);
  }

  clear = () => {
    pages.clear();
  }
}
const testLocalPageCacheStore = new LocalPageCacheStore();
export { testLocalPageCacheStore as localPageCacheStore };
