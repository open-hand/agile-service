import { axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId, getApplyType } from '@/utils/common';
import Api from './Api';

interface IStatus {
  name: string,
  description?: string,
  // organizationId: "4"
  type: string, // 阶段
}
interface UpdateData extends IStatus {
  objectVersionNumber: number
}
class StatusApi extends Api<StatusApi> {
  get orgPrefix() {
    return `/agile/v1/organizations/${this.orgId}`;
  }

  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  setProjectId(projectId?: string | number) {
    return this.overwrite('prefix', `/agile/v1/projects/${projectId || this.projectId}`);
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
  load(statusId: string) {
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
  loadByProject(applyType = getApplyType()) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/schemes/query_status_by_project_id`,
      params: { apply_type: applyType },
    });
  }

  /**
   * 根据issueId查询工作项可以转换的全部的状态
   * @param currentStatusId 当前工作项状态id
   * @param issueId 工作项id
   * @param issueTypeId 工作项类型id
   * @param applyType
   */
  loadTransformStatusByIssue(currentStatusId: number, issueId: number, issueTypeId: string, applyType: string = 'agile') {
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
   * 在该工作台查询下所有状态
   */
  loadAllFromWorkbench(params?: { page?: number, size?: number, param?: string }, data: { ignoredStatusIds?: string[], queryIgnored?: boolean } = {}) {
    return this.request({
      method: 'post',
      url: `${this.orgPrefix}/work_bench/status`,
      params,
      data,
    });
  }

  /**
   * 根据工作项类型id查询改工作项类型下所有状态
   * @param issueTypeId
   * @param applyType
   */
  loadAllForIssueType(issueTypeId: string, applyType: string = 'agile') {
    return axios({
      method: 'get',
      url: `${this.prefix}/schemes/query_status_by_issue_type_id`,
      params: {
        issue_type_id: issueTypeId,
        apply_type: applyType,
      },
    });
  }

  /**
   *查询工作流第一个状态
   * @param issueTypeId
   * @param applyType
   */
  loadFirstInWorkFlow(issueTypeId: number, applyType: string = 'agile') {
    const organizationId = getOrganizationId();
    return this.request({
      method: 'get',
      url: `${this.prefix}/status/query_first_status`,
      params: {
        organizationId,
        issueTypeId,
        applyType,
      },
    });
  }

  /**
   * 查询所有工作项类型的全部状态的对应的转换
   */
  loadAllTransformForAllIssueType = (boardId: string) => axios({
    method: 'get',
    url: `${this.prefix}/schemes/query_transforms_map`,
    params: {
      apply_type: 'agile',
      boardId,
    },
  })

  /**
     * 删除状态
     * @param statusId
     */
  delete(statusId: string) {
    return axios.delete(`${this.orgPrefix}/status/${statusId}`);
  }

  /**
   * 更新状态
   * @param statusId
   * @param updateData
   */
  update(statusId: string, updateData: UpdateData) {
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

  /**
   * 校验是否能新增状态 [敏捷]
   * @param applyType
   */
  checkCanCreateStatus(applyType = 'agile') {
    return axios({
      method: 'get',
      url: `${this.prefix}/schemes/check_create_status_for_agile`,
      params: {
        applyType,
      },
    });
  }

  /**
   * 校验是否能删除状态 [敏捷]
   * @param code
   */
  checkCanDelStatus(statusId: number, applyType = 'agile') {
    return axios({
      method: 'get',
      url: `${this.prefix}/schemes/check_remove_status_for_agile`,
      params: {
        status_id: statusId,
        applyType,
      },
    });
  }
}

const statusApi = new StatusApi();
const statusApiConfig = new StatusApi(true);
export { statusApi, statusApiConfig };
