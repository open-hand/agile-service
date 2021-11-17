import { axios } from '@choerodon/boot';
import { getOrganizationId, getProjectId } from '@/utils/common';

class ProjectApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  /**
   * 根据当前项目id得到项目信息
   */
  loadInfo() {
    return axios.get(`${this.prefix}/project_info`);
  }

  /**
 * 获取项目基本信息
 * @param projectId
 */
  loadBasicInfo(projectId?: string) {
    return axios.get(`/iam/choerodon/v1/projects/${projectId}/basic_info`);
  }

  /**
   * 查询组织下用户有权限的创建成功的启用的项目
   * @param { userId, filter, page, size, category } category表示筛选的项目类型
   */
  loadProjectByUser({
    userId, filter, page, size, category,
  }: any) {
    return axios.get(`iam/choerodon/v1/organizations/${getOrganizationId()}/users/${userId}/projects/paging?enabled=true&page=${page}&size=${size}&onlySucceed=true${filter ? `&name=${filter}` : ''}${category ? `&category=${category}` : ''}`);
  }

  /**
   * 查询组织下用户有权限的项目 （创建分支/关联分支处）
   *
   */
  loadFromBranch({
    currentProjectId, page, size, param, userId,
  }: { currentProjectId?: string, page?: number, size?: number, param?: string, userId?: string }) {
    return axios({
      url: `iam/choerodon/v1/organizations/${getOrganizationId()}/users/${userId}/page_owned_projects`,
      method: 'get',
      params: {
        current_project_id: currentProjectId,
        page: page || 1,
        size: size || 50,
        param,

      },
    });
  }
}

const projectApi = new ProjectApi();
export { projectApi };
