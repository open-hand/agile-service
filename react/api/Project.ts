import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

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
}

const projectApi = new ProjectApi();
export { projectApi };
