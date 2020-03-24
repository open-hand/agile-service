import { stores, axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '../common/utils';

export function getFeatures(pagination, searchVO, sort) {
  const { size, page } = pagination;
  return axios.post(`/agile/v1/projects/${getProjectId()}/issues/program?size=${size}&page=${page}&organizationId=${getOrganizationId()}`, searchVO, {
    params: sort,
  });
}
export function getFeaturesInProject() {
  return axios.get(`/agile/v1/projects/${getProjectId()}/issues/features?organizationId=${getOrganizationId()}`);
}

export function getFeaturesByEpic(epicId) {
  return axios.get(`/agile/v1/projects/${getProjectId()}/issues/feature/select_data?organizationId=${getOrganizationId()}${epicId ? `&epicId=${epicId}` : ''}`);
}
export function exportFeatures(search) {
  return axios.post(`/zuul/agile/v1/projects/${getProjectId()}/issues/program/export?organizationId=${getOrganizationId()}`, search, { responseType: 'arraybuffer' });
} 

export function getFeaturesColor() {
  return axios.get('/agile/v1/lookup_values/feature_color');
} 
export function getSubStoryByFeature(issueId) {
  return axios.post(`/agile/v1/projects/${getProjectId()}/issues/list_story_by_feature_id?issueId=${issueId}`);
} 
/**
 *  "teamProjectId": 1655,
 *  "featureId": 81031,
 *  "piId":57,
 *  "sprintIds": [4971]
 *
 * @export
 * @param {*} issueId
 * @returns
 */
export function updateFeatureTeamAndSprint(data) {
  return axios.post(`/agile/v1/projects/${getProjectId()}/board_feature/feature_link_project`, data);
} 
export function removeFeatureTeam({
  teamId,
  featureId,
  piId,
}) {
  return axios.delete(`/agile/v1/projects/${getProjectId()}/board_feature/delete_project_related?teamId=${teamId}&featureId=${featureId}${piId ? `&piId=${piId}` : ''}`);
<<<<<<< HEAD
}
export function checkFeatureName(summary, epicId) {
  return axios.post(`/agile/v1/projects/${getProjectId()}/issues/check_feature_summary`, {
    epicId,
    summary,
  });
}
export function checkFeatureNameById(featureId, epicId) {
  return axios.post(`/agile/v1/projects/${getProjectId()}/issues/check_feature_summary`, {
    epicId,
    featureIds: [featureId],
  });
=======
>>>>>>> ecc78d3f838596bc18e2ffc2d149ff25e6f47240
}
