import Api from './Api';
import {getOrganizationId} from "@/utils/common";

interface CreateItem {
  predecessorType: string,
  predecessorIssueTypeId: string,
  predecessorIssueStatusId: string,
}

interface LinkageItemProps {
  issueTypeId: string,
  predecessorIssueTypeId: string,
  predecessorType: string,
  statusId: string,
}

class PredecessorApi extends Api<PredecessorApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}/predecessor_issue_status_linkage`;
  }

  /**
   * 创建依赖关系联动
   * @param issueTypeId
   * @param statusId
   * @param data
   */
  createLinkage(issueTypeId: string, statusId: string, data: CreateItem[]) {
    return this.request({
      method: 'post',
      url: this.prefix,
      params: {
        issueTypeId,
        statusId,
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  /**
   * 查询依赖关系联动
   * @param issueTypeId
   * @param statusId
   */
  loadLinkage(issueTypeId: string, statusId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}`,
      params: {
        issueTypeId,
        statusId,
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 查询依赖关系可选状态
   * @param data
   */
  loadLinkageStatus(data: LinkageItemProps) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/status`,
      params: {
        organizationId: getOrganizationId(),
      },
      data,
    });
  }
}

const predecessorApi = new PredecessorApi();
const predecessorApiConfigApi = new PredecessorApi(true);
export { predecessorApi, predecessorApiConfigApi };
