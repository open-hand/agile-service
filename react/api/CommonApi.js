/* eslint-disable camelcase */
import { stores, axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '../common/utils';


const { AppState } = stores;

export function getSelf() {
  return axios.get('/base/v1/users/self');
}

export function getUsers(param, userId, page = 1) {
  const projectId = AppState.currentMenuType.id;
  if (param) {
    return axios.get(`/base/v1/projects/${projectId}/users?param=${param}${userId ? `&id=${userId}` : ''}`);
  }
  return axios.get(`/base/v1/projects/${projectId}/users?size=20&page=${page}${userId ? `&id=${userId}` : ''}`);
}
export function getUser(userId) {
  const projectId = AppState.currentMenuType.id;
  return axios.get(`base/v1/projects/${projectId}/users?id=${userId}`);
}
export async function checkPermissionProject(service) {
  const { type } = AppState.currentMenuType;
  const data = service.map(code => ({
    code,
    organizationId: getOrganizationId(),
    projectId: getProjectId(),
    resourceType: type,
  }));
  const result = await axios.post('/base/v1/permissions/checkPermission', data);
  return result && result.some(permission => permission.approve);
}

export function getProjectsInProgram() {
  return axios.get(`base/v1/organizations/${getOrganizationId()}/projects/${getProjectId()}/program`);
}

export function getProjectIsShowFeature() { // /v1/projects/1551/art/isArtDoding
  return axios.get(`agile/v1/projects/${getProjectId()}/art/isArtDoding`);
}

export function getSubProjects(only_select_enable = false) {
  return axios.get(`/base/v1/organizations/${getOrganizationId()}/project_relations/${getProjectId()}/${getProjectId()}?only_select_enable=${only_select_enable || false}`);
}

export function getIssueUsers(param, userId, page = 1) {
  const projectId = AppState.currentMenuType.id;
  if (param) {
    return axios.get(`/agile/v1/projects/${projectId}/issues/users?param=${param}${userId ? `&id=${userId}` : ''}`);
  }
  return axios.get(`/agile/v1/projects/${projectId}/issues/users?size=20&page=${page}${userId ? `&id=${userId}` : ''}`);
}
