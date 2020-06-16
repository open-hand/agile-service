import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

class DevOpsApi {
  get prefix() {
    return `/devops/v1/projects/${getProjectId()}`;
  }

  get issuePrefix() {
    return `/devops/v1/project/${getProjectId()}`;
  }

  /**
   * 统计分支相关数据
   * @param issueId 
   */
  countBranchs(issueId:number) {
    return axios.get(`${this.issuePrefix}/issue/${issueId}/commit_and_merge_request/count`);
  }

  /**
   * 根据issueId加载相关commit
   * @param issueId 
   */
  loadCommit(issueId:number) {
    return axios.get(`${this.issuePrefix}/issue/${issueId}/commit/list`);
  }

  /**
   * 加载跳转git合并链接
   * @param appServiceId 
   */
  loadGitUrl(appServiceId:number) {
    // return axios.get(`${this.prefix}/app_service/${appServiceId}/git/url`);
    return axios.get(`${this.prefix}/app_service/${appServiceId}/git/url`);
  }
}

const devOpsApi = new DevOpsApi();
export { devOpsApi };
