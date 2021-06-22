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

  openProjectPrefix=true;

  get project() {
    return this.openProjectPrefix ? `${getProjectId()} ` : '';
  }

  constructor(cacheStore: CacheStoreInterface, config?: CacheBaseStoreConfigProps) {
    this.cacheStore = cacheStore;
    this.openProjectPrefix = config?.openProjectPrefix ?? true;
  }

  getItem = (code: T) => this.cacheStore.getItem(`${this.project}${code}`)

  setItem = (code: T, data: any) => this.cacheStore.setItem(`${this.project}${code}`, data);

  removeItem = (code: T) => this.cacheStore.removeItem(`${this.project}${code}`);

  remove = (code: T) => this.removeItem(`${this.project}${code}` as T);

  has = (code: any) => (typeof (this.cacheStore.has) === 'function' ? this.cacheStore.has(`${this.project}${code}`) : this.getItem(`${this.project}${code}` as T))

  clear = () => this.cacheStore.clear()
}
export default CacheBaseStore;
