import { omit, set } from 'lodash';
import { getProjectId } from '@/utils/common';

export interface CacheStoreInterface {
  getItem: (code: string) => any

  setItem: (code: string, data: any) => void

  removeItem: (code: string) => void

  clear: () => void

  [propsName: string]: any
}
interface CacheBaseStoreConfigProps {
  openProjectPrefix?: boolean /** 是否启用项目前缀 */
}

/**
 * 缓存store
 */
class CacheBaseStore<T extends string> implements CacheStoreInterface {
  cacheStore: CacheStoreInterface

  openProjectPrefix = true;

  [propsName: string]: any;

  get project() {
    return this.openProjectPrefix ? `${getProjectId()} ` : '';
  }

  overwrite(Property: string, value: any): CacheBaseStore<T> {
    // 以当前this为模板，创建一个新对象
    const temp = Object.create(this);
    // 不直接temp[Property] = value;的原因是，如果这个属性只有getter，会报错
    Object.defineProperty(temp, Property, {
      get() {
        return value;
      },
    });
    // 返回新对象
    return temp;
  }

  unPrefix() {
    return this.overwrite('project', '');
  }

  constructor(cacheStore: CacheStoreInterface, config?: CacheBaseStoreConfigProps) {
    this.cacheStore = cacheStore;

    this.openProjectPrefix = config?.openProjectPrefix ?? true;
    const otherProps = omit(cacheStore, 'getItem', 'setItem', 'removeItem', 'clear', 'has', 'remove');
    Object.entries(otherProps).forEach(([key, value]) => {
      if (value instanceof Function) {
        set(this, key, (...args: any[]) => this.cacheStore[key](...args));
      }
    });
  }

  getItem(code: T) { return this.cacheStore.getItem(`${this.project}${code}`); }

  setItem(code: T, data: any) { this.cacheStore.setItem(`${this.project}${code}`, data); }

  removeItem(code: T) { this.cacheStore.removeItem(`${this.project}${code}`); }

  remove(code: T) { this.removeItem(`${this.project}${code}` as T); }

  has(code: any) { return (typeof (this.cacheStore.has) === 'function' ? this.cacheStore.has(`${this.project}${code}`) : this.getItem(`${this.project}${code}` as T)); }

  clear() { this.cacheStore.clear(); }
}

export default CacheBaseStore;
