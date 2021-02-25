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
const pageKeys = new Map<string, Set<string>>();
class LocalPageCacheStore implements LocalPageCacheStoreInterface {
  pageKeyList = ['scrumboard', 'issues']; //

  setItem(pageKey: string, data: any) {
   pageKeys.get(getProjectId())?.add(pageKey) || pageKeys.set(getProjectId(), new Set([pageKey]));
   pages.set(`${getProjectId()}-${pageKey}`, data);
  }

  mergeSetItem<T extends object>(pageKey: string, data: T) {
    pageKeys.get(getProjectId())?.add(pageKey) || pageKeys.set(getProjectId(), new Set([pageKey]));
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
    pageKeys.get(getProjectId())?.delete(pageKey);
    pages.delete(`${getProjectId()}-${pageKey}`);
  }

  has(pageKey: string | RegExp) {
    const projectId = getProjectId();
    if (typeof (pageKey) === 'string' && pages.has(`${projectId}-${pageKey}`)) {
      return true;
    }
    if (Object.prototype.toString.call(pageKey) === '[object RegExp]' && !!pageKeys.get(getProjectId())?.size) {
      const pageKeyList = Array.from(pageKeys.get(getProjectId())!);
      return pageKeyList.some((key) => (pageKey as RegExp).test(key));
    }
    return false;
  }

  clear = () => {
    pages.clear();
    pageKeys.clear();
  }
}
const testLocalPageCacheStore = new LocalPageCacheStore();
export { testLocalPageCacheStore as localPageCacheStore };
