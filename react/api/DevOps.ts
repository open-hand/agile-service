import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

class DevOpsApi {
  get prefix() {
    return `/devops/v1/project/${getProjectId()}`;
  }

  /**
   * 统计分支相关数据
   * @param issueId 
   */
  countBranchs(issueId:number) {
    return axios.get(`${this.prefix}/issue/${issueId}/commit_and_merge_request/count`);
  }
}

const devOpsApi = new DevOpsApi();
export { devOpsApi };
