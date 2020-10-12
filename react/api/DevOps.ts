import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import Api from './Api';

interface ICreateBranch {
  branchName: string,
  issueId: number,
  originBranch: string,
}
class DevOpsApi extends Api<DevOpsApi> {
  get prefix() {
    return `/devops/v1/projects/${this.projectId}`;
  }

  get issuePrefix() {
    return `/devops/v1/project/${this.projectId}`;
  }

  /**
   * 创建分支
   * @param applicationId
   * @param devopsBranchVO
   */
  createBranch(applicationId: number, devopsBranchVO: ICreateBranch) {
    return axios.post(`${this.prefix}/app_service/${applicationId}/git/branch`, devopsBranchVO);
  }

  /**
   * 统计分支相关数据
   * @param issueId
   */
  countBranches(issueId: number) {
    return this.request({
      method: 'get',
      url: `${this.issuePrefix}/issue/${issueId}/commit_and_merge_request/count`,
    });
  }

  /**
   * 根据issueId加载相关commit
   * @param issueId
   */
  loadCommit(issueId: number) {
    return axios.get(`${this.issuePrefix}/issue/${issueId}/commit/list`);
  }

  /**
   * 加载已经启用的服务列表
   */
  loadActiveService() {
    return axios.get(`${this.prefix}/app_service/list_by_active`);
  }

  /**
   * 根据服务id加载分支
   * @param applicationId
   * @param page
   * @param size
   * @param searchVO
   */
  loadBranchesByService(applicationId: number, page: number = 1, size: number = 5, searchVO: any) {
    return axios({
      method: 'post',
      url: `${this.prefix}/app_service/${applicationId}/git/page_branch_by_options`,
      params: {
        page,
        size,
        sort: 'creation_date,asc',
      },
      data: searchVO,
    });
  }

  /**
   * 根据服务id加载标签列表
   * @param applicationId
   * @param page
   * @param size
   * @param searchVO
   */
  loadTagsByService(applicationId: number, page: number = 1, size: number = 5, searchVO: any) {
    return axios({
      method: 'post',
      url: `${this.prefix}/app_service/${applicationId}/git/page_tags_by_options`,
      params: {
        page,
        size,
      },
      data: searchVO,
    });
  }

  /**
   * 根据issueId获取问题关联的请求合并列表
   * @param issueId
   */
  loadMergeRequest(issueId:number) {
    return axios.get(`${this.issuePrefix}/issue/${issueId}/merge_request/list`);
  }

  /**
   * 加载跳转git合并链接
   * @param appServiceId
   */
  loadGitUrl(appServiceId: number) {
    // return axios.get(`${this.prefix}/app_service/${appServiceId}/git/url`);
    return axios.get(`${this.prefix}/app_service/${appServiceId}/git/url`);
  }
}

const devOpsApi = new DevOpsApi();
export { devOpsApi };
