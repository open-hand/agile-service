import { merge } from 'lodash';

interface LocalPageCacheStoreInterface {
  getItem: (code: string) => any,
  setItem: (code: string, data: any) => void,
  remove: (code: string) => void,
  clear: () => void,
}
const pages = new Map<string, any>();
class LocalPageCacheStore implements LocalPageCacheStoreInterface {
  pageKeyList = ['scrumboard', 'issues']

  setItem(pageKey: string, data: any) {
    pages.set(pageKey, data);
  }

  mergeSetItem<T extends object>(pageKey: string, data: T) {
    pages.set(pageKey, merge(pages.get(pageKey), data));
  }

  getItem(pageKey: string) {
    return pages.get(pageKey);
  }

  remove(pageKey: string) {
    pages.delete(pageKey);
  }

  clear = () => {
    pages.clear();
  }
}
const testLocalPageCacheStore = new LocalPageCacheStore();
export { testLocalPageCacheStore as localPageCacheStore };
