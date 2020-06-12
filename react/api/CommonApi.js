/* eslint-disable camelcase */
import { stores, axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '@/utils/common';


const { AppState } = stores;


export function getProjectsInProgram() {
  return axios.get(`iam/choerodon/v1/organizations/${getOrganizationId()}/projects/${getProjectId()}/program`);
}

export function getSubProjects(only_select_enable = false) {
  return axios.get(`/iam/choerodon/v1/organizations/${getOrganizationId()}/project_relations/${getProjectId()}/${getProjectId()}?only_select_enable=${only_select_enable || false}`);
}

export async function getIsOwner() {
  const projectId = AppState.currentMenuType.id;
  const userId = AppState.userInfo.id;
  const roles = await axios.get(`/iam/choerodon/v1/projects/${projectId}/role_members/users/${userId}`);
  return roles.some(role => role.code === 'project-admin');
}
