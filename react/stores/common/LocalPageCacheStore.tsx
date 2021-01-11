import { getProjectId } from '@/utils/common';
import { toJS } from 'mobx';
import { merge, omit } from 'lodash';

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
    pages.set(`${getProjectId()}-${pageKey}`, data);
  }

  mergeSetItem<T extends object>(pageKey: string, data: T) {
    const oldData = pages.get(`${getProjectId()}-${pageKey}`);
    const omitKeys = [];
    if (typeof (oldData) === 'object' && typeof (data) === 'object') {
      for (const [key, value] of Object.entries(data)) {
        if (typeof (value) === 'undefined' || (Array.isArray(toJS(value)) && value.length === 0)) {
          omitKeys.push(key);
        }
      }
    }
    const newData = merge(omit(oldData, omitKeys), data);
    pages.set(`${getProjectId()}-${pageKey}`, newData);
  }

  getItem(pageKey: string) {
    return pages.get(`${getProjectId()}-${pageKey}`);
  }

  remove(pageKey: string) {
    pages.delete(`${getProjectId()}-${pageKey}`);
  }

  clear = () => {
    pages.clear();
  }
}
const testLocalPageCacheStore = new LocalPageCacheStore();
export { testLocalPageCacheStore as localPageCacheStore };
