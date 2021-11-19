import { axios } from '@choerodon/boot';
import { getOrganizationId, getProjectId } from '@/utils/common';
import Api from './Api';

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
    return axios.get(`iam/choerodon/v1/organizations/${getOrganizationId()}/users/${userId}/projects/paging?enabled=true&page=${page}&size=${size}&onlySucceed=true${filter ? `&name=${filter}` : ''}${category ? `&category=${category}` : ''}`);
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
        param,
        page,
        size,
      },
      data: {
        filterProjectIds,
        enabled,
      },
    });
  }
}

const projectApi = new ProjectApi();
export { projectApi };
