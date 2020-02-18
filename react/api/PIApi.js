import { stores, axios } from '@choerodon/boot';

const { AppState } = stores;

/**
 * 获取PI列表
 */
// eslint-disable-next-line import/prefer-default-export
export function getPISelect() {
  return axios.get(`/agile//v1/projects/${AppState.currentMenuType.id}/pi/unfinished`);
}
