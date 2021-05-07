import { getProjectId } from '@/utils/common';
import Api from './Api';

interface ILinkTypeQuery {
  page?: number,
  size?: number,
  issueLinkTypeId?: number,
  filter?: {
    params: string,
    linkName: string,
  },
}

class IssueLinkTypeApi extends Api<IssueLinkTypeApi> {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  getAll({
    page = 1,
    size = 999,
    issueLinkTypeId,
    filter,
  }: ILinkTypeQuery = {}, projectId?: number) {
    return this.request({
      url: `/agile/v1/projects/${projectId || getProjectId()}/issue_link_types/query_all`,
      method: 'POST',
      data: {
        linkName: filter?.linkName,
        contents: filter?.params ? [filter.params] : undefined,
      },
      params: {
        page,
        size,
        issueLinkTypeId,
      },
    });
  }

  /**
   * 检查issueLinkType 是否重名
   * @param issueLinkTypeName
   * @param issueLinkTypeId
   */
  checkName(issueLinkTypeName:string, issueLinkTypeId?:number) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/issue_link_types/check_name`,
      params: {
        issueLinkTypeName,
        issueLinkTypeId,
      },
    });
  }
}

const issueLinkTypeApi = new IssueLinkTypeApi();
const issueLinkTypeApiConfig = new IssueLinkTypeApi(true);
export { issueLinkTypeApi, issueLinkTypeApiConfig };
