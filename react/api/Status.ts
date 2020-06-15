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
  get orgPrefix() {
    return `/agile/v1/organizations/${getOrganizationId()}`;
  }

  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  /**
    * 创建状态
    * @param map 
    */
  create(data: IStatus) {
    return axios.post(`${this.orgPrefix}/status`, data);
  }

  /**
    * 根据状态id加载状态
    * @param statusId 
    */
  load(statusId: number) {
    return axios.get(`${this.orgPrefix}/status/${statusId}`);
  }


  /**
    * 加载状态列表 
    * @param page 
    * @param size 
    * @param sort 
    * @param filters 
    */
  loadList(page: number = 1, size: number = 20, sort: string, filters: object) {
    return axios({
      method: 'post',
      url: `${this.orgPrefix}/status/list`,
      params: {
        page,
        size,
        sort,
      },
      data: filters,
    });
  }

  /**
   * 加载当前项目下所有状态
   * @param applyType 
   */
  loadByProject(applyType = 'agile') {
    return axios.get({
      method: 'get',
      url: `${this.prefix}/schemes/query_status_by_project_id`,
      params: { apply_type: applyType },
    });
  }

  /**
   * 根据issueId查询问题可以转换的全部的状态
   * @param currentStatusId 当前问题状态id 
   * @param issueId 问题id
   * @param issueTypeId 问题类型id
   * @param applyType 
   */
  loadTransformStatusByIssue(currentStatusId:number, issueId:number, issueTypeId:string, applyType:string = 'agile') {
    return axios({
      method: 'get',
      url: `${this.prefix}/schemes/query_transforms`,
      params: {
        current_status_id: currentStatusId,
        issue_id: issueId,
        issue_type_id: issueTypeId,
        apply_type: applyType,
      },
    });
  }

  /**
   * 查询该组织下所有状态
   */
  loadAll() {
    return axios.get(`${this.orgPrefix}/status/query_all`);
  }

  /**
     * 删除状态
     * @param statusId 
     */
  delete(statusId: number) {
    return axios.delete(`${this.orgPrefix}/status/${statusId}`);
  }

  /**
   * 更新状态
   * @param statusId 
   * @param updateData 
   */
  update(statusId: number, updateData: UpdateData) {
    return axios.put(`${this.orgPrefix}/status/${statusId}`, updateData);
  }

  /**
    * 检查状态名是否重复
    * @param name 
    */
  checkName(name: string) {
    return axios({
      method: 'get',
      url: `${this.orgPrefix}/status/check_name`,
      params: {
        name,
      },
    });
  }
}

const statusApi = new StatusApi();
export { statusApi };
