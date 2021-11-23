/* eslint-disable camelcase */
import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import Api from './Api';

interface ICreateBranch {
  branchName: string,
  issueId: number | string,
  originBranch: string,
}
interface ILinkBrach {
  appServiceId: string
  issueIds: Array<string | number>
  objectVersionNumber: number
  branchName: string
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
    return this.request({
      method: 'post',
      url: `${this.prefix}/app_service/${applicationId}/git/branch`,
      data: devopsBranchVO,
    });
  }

  linkBranch(applicationId: number, devopsBranchVO: ILinkBrach) {
    return this.request({
      method: 'put',
      url: `${this.prefix}/app_service/${applicationId}/git/update_branch_issue`,
      data: devopsBranchVO,
      params: {
        onlyInsert: true,
      },
    });
  }

  checkBranchName(applicationId: number, branchName: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/app_service/${applicationId}/git/check_branch_name`,
      params: {
        branch_name: branchName,
      },
    });
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
  loadCommit(issueId: string) {
    return this.request({
      method: 'get',
      url: `${this.issuePrefix}/issue/${issueId}/commit/list`,
    });
  }

  /**
   * 加载已经启用的服务列表
   */
  loadActiveService(checkMember?: boolean) {
    return this.request({
      url: `${this.prefix}/app_service/list_by_active`,
      method: 'get',
      params: {
        checkMember,
      },
    });
  }

  /**
   * 加载已经启用的服务列表
   */
  loadPageActiveService(params: { targetProjectId: string, param?: string, page?: number, size?: number }) {
    return this.request({
      url: `${this.prefix}/app_service/page_by_active`,
      method: 'get',
      params: {
        ...params,
        target_project_id: params.targetProjectId,
      },
    });
  }

  /**
   * 加载已经启用的服务列表
   */
  loadProjectActiveService(page: number, size: number, param?: string) {
    return this.request({
      url: `${this.prefix}/app_service/list_service_under_org`,
      method: 'get',
      params: {
        page,
        size,
        param,
      },
    });
  }

  /**
   * 根据服务id加载分支
   * @param applicationId
   * @param page
   * @param size
   * @param searchVO
   */
  loadBranchesByService(applicationId: number, page: number = 1, size: number = 5, searchVO: any) {
    return this.request({
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

  loadBranchesByServiceFilterIssue(applicationId: number | string, page: number = 1, size: number = 5, searchVO: any, issue_id: string) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/app_service/${applicationId}/git/page_branch_by_options_filtered_by_issue_id`,
      params: {
        page,
        size,
        sort: 'creation_date,asc',
        issue_id,
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
  loadTagsByService(applicationId: number | string, page: number = 1, size: number = 5, searchVO: {
    param?: string
    searchParam?: { tagName?: string }
  }, checkMember?: boolean) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/app_service/${applicationId}/git/page_tags_by_options`,
      params: {
        page,
        size,
        checkMember,
      },
      data: searchVO,
    });
  }

  removeLinkBranch(applicationId: number, branchId: string, issueId: string) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/app_service/${applicationId}/git/branch/issue/remove_association`,
      params: {
        branch_id: branchId,
        issue_id: issueId,
      },
    });
  }

  /**
   * 根据issueId获取工作项关联的请求合并列表
   * @param issueId
   */
  loadMergeRequest(issueId: number) {
    return this.request({
      url: `${this.issuePrefix}/issue/${issueId}/merge_request/list`,
      method: 'get',
    });
  }

  /**
   * 加载跳转git合并链接
   * @param appServiceId
   */
  loadGitUrl(appServiceId: number) {
    // return axios.get(`${this.prefix}/app_service/${appServiceId}/git/url`);
    return this.request({
      url: `${this.prefix}/app_service/${appServiceId}/git/url`,
      method: 'get',

    });
  }
}

const devOpsApi = new DevOpsApi();
const devOpsApiConfig = new DevOpsApi(true);
export { devOpsApi, devOpsApiConfig };
