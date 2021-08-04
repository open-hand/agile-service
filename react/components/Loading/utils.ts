import { set } from 'lodash';
import { ILoadingChangeItem, ILoadingChildren } from './type';
/**
 * 判断子Loading是否为自更新Loading
 * @param newStatusItem
 * @param childrenLoad
 * @returns
 */
function filterSelfLoading(newStatusItem: ILoadingChangeItem, childrenLoad?: ILoadingChildren) {
  const allowSelfLoading = childrenLoad?.allowSelfLoading || newStatusItem.allowSelfLoading;
  if (!childrenLoad) {
    return false;
  }
  if (childrenLoad.finishInit) {
    return !!allowSelfLoading;
  }
  if (newStatusItem.status !== childrenLoad.initStatus) {
    set(childrenLoad, 'finishInit', true);
    return false;
  }
  return false;
}

export { filterSelfLoading };
