import { axios } from '@choerodon/boot';
import {getOrganizationId, getProjectId} from '@/utils/common';

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
   * 查询组织下用户有权限的项目
   * @param userId
   */
  loadProjectByUser({ userId, filter, page, size }) {
    return axios.get(`iam/choerodon/v1/organizations/${getOrganizationId()}/users/${userId}/projects/paging?page=${page}&size=${size}`);
  }
}

const projectApi = new ProjectApi();
export { projectApi };
