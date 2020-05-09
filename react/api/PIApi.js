import { stores, axios } from '@choerodon/boot';

const { AppState } = stores;

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
