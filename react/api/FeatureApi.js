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
