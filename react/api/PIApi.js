import { stores, axios } from '@choerodon/boot';

const { AppState } = stores;

/**
 * 获取PI列表
 */
// eslint-disable-next-line import/prefer-default-export
export function getPISelect() {
  return axios.get(`/agile//v1/projects/${AppState.currentMenuType.id}/pi/unfinished`);
}
/**
 * 获取PI列表
 */
export function getAllPIList(statusList = ['todo', 'doing', 'done']) {
  return axios.post(`/agile/v1/projects/${AppState.currentMenuType.id}/pi/query_pi_by_status`, statusList);
}
export function changeIssuePI(issueId, sourceId, destinationId) {
  return axios.post(`agile/v1/projects/${AppState.currentMenuType.id}/pi/to_pi/${destinationId}`, {
    before: false,
    issueIds: [issueId],
    outsetIssueId: 0,
    rankIndex: 0,
    currentPiId: sourceId,
  });
}
export function getTeamSprints(piId, teamIds) {
  return axios.get(`agile/v1/projects/${AppState.currentMenuType.id}/sprint/sub_project/list_by_team_ids?piId=${piId}&teamIds=${teamIds.join(',')}`);
}
