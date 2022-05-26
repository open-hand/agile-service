import { axios } from '@choerodon/boot';
import { getOrganizationId, getProjectId } from '@/utils/common';
import Api from './Api';

export interface IProjectInfo {
  infoId: string,
  objectVersionNumber: number,
  hidePreSprintDoneSubissue: boolean
}
class ProjectApi extends Api<ProjectApi> {
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
    return this.request({
      method: 'get',
      url: `iam/choerodon/v1/organizations/${this.orgId}/users/${userId}/projects/paging`,
      params: {
        page,
        size,
        onlySucceed: true,
        name: filter || '',
        category,
      },
    });
  }

  /**
   * 工作台加载项目列表 分页
   * @param param0
   * @returns
   */
  loadProjectForWorkbench({
    userId, param, page, size, enabled = true, filterProjectIds,
  }: any) {
    return this.request({
      method: 'post',
      url: `iam/choerodon/v1/organizations/${this.orgId}/users/${userId}/projects/paging_option`,
      params: {
        page,
        size,
        params: param,
      },
      data: {
        filterProjectIds,
        enabled,
      },
    });
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

  updateProject(data: IProjectInfo) {
    return this.request({
      method: 'put',
      url: `${this.prefix}/project_info`,
      data,
    });
  }

  /**
   * 查询组织下有权限的项目
   * @param onlyBacklog true 需求 false 敏捷/项目群/瀑布
   */
  loadProjectByCategory({
    onlyBacklog = false, page, size, param,
  }: { onlyBacklog?: boolean, page?: number, size?: number, param?: string }) {
    return this.request({
      method: 'post',
      url: `agile/v1/organizations/${this.orgId}/projects/list_by_category`,
      params: {
        page: page || 1,
        size: size || 50,
        onlyBacklog,
      },
      data: {
        enabled: true,
        param,
      },
    });
  }
}

const projectApi = new ProjectApi();
export { projectApi };
