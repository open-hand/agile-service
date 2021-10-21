import { axios } from '@choerodon/boot';
import { getProjectId, getMenuType } from '@/utils/common';
import { sameProject } from '@/utils/detail';
import Api from './Api';

export interface IIssueLink {
  linkTypeId: string, // 链接类型id
  linkedIssueId: string, // 被链接工作项id
  issueId: string;// 链接工作项id
}

class IssueLinkApi extends Api<IssueLinkApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  get outPrefix() {
    return '/agile/v1/backlog_external';
  }

  get isOutside() {
    return false;
  }

  outside(outside: boolean) {
    return this.overwrite('isOutside', outside);
  }

  /**
   * 根据issueId及applyType加载工作项链接
   * @param issueId
   * @param applyType project
   */
  loadByIssueAndApplyType(issueId: number, applyType = 'project', projectId?: string) {
    if (applyType === 'project') {
      return this.isOutside ? this.request({
        method: 'get',
        url: `${this.outPrefix}/issue_links/${issueId}`,
        params: {
          project_id: this.projectId,
          organizationId: this.orgId,work
        },
      }) : this.request({
        method: 'get',
        url: `/agile/v1/projects/${projectId || this.projectId}/${sameProject(this.projectId) ? '' : 'project_invoke_agile/'}issue_links/${issueId}`,
        params: {
          organizationId: this.orgId,
          instanceProjectId: this.projectId,
        },
      });
    }
    return this.request({
      method: 'get',
      url: `${this.prefix}/board_depend/feature_depend/${issueId}`,
    });
  }

  /**
   * 创建工作项链接
   * @param issueId
   * @param issueLinkCreateVOList 关联的工作项
   */
  create(issueId: string, issueLinkCreateVOList: Array<IIssueLink>) {
    return this.request({
      url: `${this.prefix}/issue_links/${issueId}`,
      method: 'post',
      data: issueLinkCreateVOList,
    });
  }

  /**
   * 删除工作项链接
   * @param issueLinkId
   */
  delete(issueLinkId: number) {
    return axios.delete(`${this.prefix}/issue_links/${issueLinkId}`);
  }
}

const issueLinkApi = new IssueLinkApi();
export { issueLinkApi };
