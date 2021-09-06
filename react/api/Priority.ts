import { axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '@/utils/common';
import Api from './Api';

export interface IPriority {
  name: string
  description: string,
  default: boolean, // 是否设置为默认优先级
  colour: string,
}
export interface UPriority extends IPriority {
  objectVersionNumber: number,
}
export interface ISequence {
  id: number,
  sequence: number,
}
class PriorityApi extends Api<PriorityApi> {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  get orgPrefix() {
    return `/agile/v1/organizations/${getOrganizationId()}`;
  }

  /**
   * 加载优先级列表
   */
  load(data: { name?: string, description?: string, params?: string }) {
    return this.request({
      method: 'get',
      url: `${this.orgPrefix}/priority`,
      params: {
        param: data?.params,
        name: data?.name,
        description: data?.description,
      },
    });
  }

  /**
     * 根据项目id查询组织默认优先级
     */
  getDefaultByProject() {
    return axios.get(`${this.prefix}/priority/default`);
  }

  /**
     * 根据项目id查询组织优先级
     */
  loadByProject(projectId?: string, priorityIds?: string[]) {
    // 进行一层处理，过滤掉禁用的优先级
    return axios.get(`/agile/v1/projects/${projectId || getProjectId()}/priority/list_by_org`)
      .then((data: any) => Array.isArray(data)
       && data.filter((v) => v.enable
       || (Array.isArray(priorityIds) && priorityIds.some((id) => id === v.id))));
  }

  /**
     * 创建优先级
     * @param priority
     */
  create(priority: IPriority) {
    return axios.post(`${this.orgPrefix}/priority`, priority);
  }

  /**
   * 更新优先级
   * @param priorityId
   * @param priority
   */
  update(priorityId: string, priority: UPriority) {
    return axios.put(`${this.orgPrefix}/priority/${priorityId}`, priority);
  }

  /**
   * 更改优先级状态
   * @param priorityId
   * @param enable
   */
  updateStatus(priorityId: number, enable: boolean) {
    return axios({
      method: 'get',
      url: `${this.orgPrefix}/priority/enable/${priorityId}`,
      params: {
        enable,
      },
    });
  }

  /**
   *根据priorityId删除优先级
   * @param priorityId
   * @param changePriorityId
   */
  delete(priorityId: string, changePriorityId: string) {
    return axios({
      method: 'delete',
      url: `${this.orgPrefix}/priority/delete/${priorityId}`,
      params: {
        changePriorityId,
      },
    });
  }

  /**
   * 删除前检查优先级
   * @param priorityId
   */
  checkBeforeDel(priorityId: number) {
    return axios.get(`${this.orgPrefix}/priority/check_delete/${priorityId}`);
  }

  /**
     * 检查优先级名称是否重复
     * @param name
     */
  checkName(name: string) {
    return axios({
      method: 'get',
      url: `${this.orgPrefix}/priority/check_name`,
      params: {
        name,
      },
    });
  }

  /**
   * 优先级排序
   * @param sequences
   */
  sort(sequences: Array<ISequence>) {
    return axios.put(`${this.orgPrefix}/priority/sequence`, sequences);
  }

  /**
   * 根据冲刺id查询优先级分布状况
   * @param sprintId
   */
  getDistribute(sprintId: number) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'get',
      url: `${this.prefix}/iterative_worktable/priority`,
      params: {
        sprintId,
        organizationId,
      },
    });
  }
}

const priorityApi = new PriorityApi();
const priorityApiConfig = new PriorityApi(true);
export { priorityApi, priorityApiConfig };
