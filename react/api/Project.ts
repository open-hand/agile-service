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
}

const projectApi = new ProjectApi();
export { projectApi };
