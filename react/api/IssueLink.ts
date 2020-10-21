import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import Api from './Api';

interface IIssueLink {
    linkTypeId: number, // 链接类型id
    linkedIssueId: number, // 被链接问题id
    issueId: number;// 链接问题id
}

class IssueLinkApi extends Api<IssueLinkApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  /**
   * 根据issueId及applyType加载问题链接
   * @param issueId
   * @param applyType project
   */
  loadByIssueAndApplyType(issueId:number, applyType = 'project') {
    // eslint-disable-next-line no-cond-assign
    if (applyType === 'project') {
      return this.request({
        method: 'get',
        url: `${this.prefix}/issue_links/${issueId}`,
      });
    }
    return this.request({
      method: 'get',
      url: `${this.prefix}/board_depend/feature_depend/${issueId}`,
    });
  }

  /**
   * 创建问题链接
   * @param issueId
   * @param issueLinkCreateVOList 关联的问题
   */
  create(issueId:number, issueLinkCreateVOList:Array<IIssueLink>) {
    return axios.post(`${this.prefix}/issue_links/${issueId}`, issueLinkCreateVOList);
  }

  /**
   * 删除问题链接
   * @param issueLinkId
   */
  delete(issueLinkId:number) {
    return axios.delete(`${this.prefix}/issue_links/${issueLinkId}`);
  }
}

const issueLinkApi = new IssueLinkApi();
export { issueLinkApi };
