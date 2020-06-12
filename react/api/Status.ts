import { axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '@/utils/common';

interface IStatus {
    name: string,
    description?: string,
    // organizationId: "4"
    type: string, // 状态类型
}
interface UpdateData extends IStatus {
    objectVersionNumber: number
}
class StatusApi {
  get prefix() {
    return `/agile/v1/organizations/${getOrganizationId()}`;
  }

  /**
         * 创建状态
         * @param map 
         */
  create(data: IStatus) {
    return axios.post(`${this.prefix}/status`, data);
  }

  /**
     * 根据状态id加载状态
     * @param statusId 
     */
  load(statusId: number) {
    return axios.get(`${this.prefix}/status/${statusId}`);
  }


  /**
         * 加载状态列表
         * @param page 
         * @param size 
         * @param sort 
         * @param filters 
         */
  loadStatusList(page: number = 1, size: number = 20, sort: string, filters: object) {
    return axios({
      method: 'post',
      url: `${this.prefix}/status/list`,
      params: {
        page,
        size,
        sort,
      },
      data: filters,
    });
  }

  /**
   * 查询该组织下所有状态
   */
  loadAll() {
    return axios.get(`${this.prefix}/status/query_all`);
  }

  /**
     * 删除状态
     * @param statusId 
     */
  delete(statusId: number) {
    return axios.delete(`${this.prefix}/status/${statusId}`);
  }

  /**
   * 更新状态
   * @param statusId 
   * @param updateData 
   */
  update(statusId: number, updateData: UpdateData) {
    return axios.put(`${this.prefix}/status/${statusId}`, updateData);
  }

  /**
         * 检查状态名是否重复
         * @param name 
         */
  checkName(name: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/status/check_name`,
      params: {
        name,
      },
    });
  }
}

const statusApi = new StatusApi();
export { statusApi };
