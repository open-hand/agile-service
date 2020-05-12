import { axios } from '@choerodon/boot';
import { getProjectId } from '@/common/utils';

interface ILinkTypeQuery {
  page?: number,
  size?: number,
  issueLinkTypeId?: number,
  filter?: {
    contents: string[],
    linkName: string,
  },
}
class IssueLinkTypeApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }
  
  getAll({
    page = 1,
    size = 999,
    issueLinkTypeId,
    filter = {
      contents: [],
      linkName: '',
    },
  }: ILinkTypeQuery = {}) {
    return axios({
      url: `${this.prefix}/issue_link_types/query_all`,
      method: 'POST',
      params: {
        page,
        size,
        issueLinkTypeId
      },
      data: filter
    })
  }
}

const issueLinkTypeApi = new IssueLinkTypeApi();
export { issueLinkTypeApi };
