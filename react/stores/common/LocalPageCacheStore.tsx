import { merge } from 'lodash';

interface LocalPageCacheStoreInterface {
  getItem: (code: string) => any,
  setItem: (code: string, data: any) => void,
  remove: (code: string) => void,
  clear: () => void,
}

class LocalPageCacheStore implements LocalPageCacheStoreInterface {
  pageKeyList = ['scrumboard', 'issues']

  pages = new Map<string, any>();

  setItem(pageKey: string, data: any) {
    this.pages.set(pageKey, data);
  }

  mergeSetItem<T extends object>(pageKey: string, data: T) {
    this.pages.set(pageKey, merge(this.pages.get(pageKey), data));
  }

  getItem(pageKey: string) {
    return this.pages.get(pageKey);
  }

  remove(pageKey: string) {
    this.pages.delete(pageKey);
  }

  clear() {
    this.pages.clear();
  }
}
const testLocalPageCacheStore = new LocalPageCacheStore();
export { testLocalPageCacheStore as localPageCacheStore };
