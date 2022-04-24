import { axios } from '@choerodon/boot';
import Api from './Api';
import { getIsOrganization } from '@/utils/common';

export interface ListLayoutColumnVO {
  fieldId?: string,
  columnCode: string,
  width?: number,
  display: boolean,
  sort: number
}
export interface IListLayout {
  applyType: string,
  listLayoutColumnRelVOS: ListLayoutColumnVO[]
}
class CacheColumnApi extends Api<CacheColumnApi> {
  get prefix() {
    return getIsOrganization() ? `/agile/v1/organizations/${this.orgId}` : `/agile/v1/projects/${this.projectId}`;
  }

  getDefault(code: string): Promise<IListLayout> {
    return this.request({
      method: 'get',
      url: `${this.prefix}/list_layout/${code}`,
      params: {
        organizationId: this.orgId,
      },
    });
  }

  update(data: IListLayout) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/list_layout`,
      params: {
        organizationId: this.orgId,
      },
      data,
    });
  }
}

const cacheColumnApi = new CacheColumnApi();
const cacheColumnApiConfig = new CacheColumnApi(true);
export { cacheColumnApi, cacheColumnApiConfig };
