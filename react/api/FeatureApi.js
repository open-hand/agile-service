import { stores, axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '../common/utils';

export function getFeatures(pagination, searchVO, sort) {
  const { size, page } = pagination;
  return axios.post(`/agile-pro/v1/projects/${getProjectId()}/issues/program?size=${size}&page=${page}&organizationId=${getOrganizationId()}`, searchVO, {
    params: sort,
  });
}
export function getFeaturesInProject() {
  return axios.get(`/agile-pro/v1/projects/${getProjectId()}/issues/features?organizationId=${getOrganizationId()}`);
}

export function getFeaturesByEpic(epicId) {
  return axios.get(`/agile-pro/v1/projects/${getProjectId()}/issues/feature/select_data?organizationId=${getOrganizationId()}${epicId ? `&epicId=${epicId}` : ''}`);
}
export function exportFeatures(search) {
  return axios.post(`/zuul/agile-pro/v1/projects/${getProjectId()}/issues/program/export?organizationId=${getOrganizationId()}`, search, { responseType: 'arraybuffer' });
}
