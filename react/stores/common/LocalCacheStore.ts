import CacheBaseStore from './CacheBaseStore';

type LocalCacheStoreIssueTypeKeys='agile.issue.type.common.selected' | 'agile.issue.type.sub.selected'
const localCacheStore = new CacheBaseStore<LocalCacheStoreIssueTypeKeys>(localStorage);
export default localCacheStore;
