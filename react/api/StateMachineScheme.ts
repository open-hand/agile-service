import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import { getOrganizationId } from '@/utils/common';

interface IPublishScheme {
    issueTypeId: number,
    newStateMachineId: number,
    oldStateMachineId: number,
    statusChangeItems: Array<{
        oldStatus: { id: number }, newStatus: { id: number }
    }>
}
interface UScheme {
    name: string,
    objectVersionNumber: number
}
class StateMachineSchemeApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  get orgPrefix() {
    return `/agile/v1/organizations/${getOrganizationId()}`;
  }

  /**
     * 创建状态机方案
     * @param stateMachineScheme 
     */
  create(stateMachineScheme: object) {
    return axios({
      method: 'post',
      url: `${this.orgPrefix}/state_machine_scheme`,
      data: stateMachineScheme,
    });
  }

  /**
    * 创建方案配置
    * @param schemeId 
    * @param stateMachineId 
    * @param schemeVOS 
    */
  createConfig(schemeId: number, stateMachineId: number, schemeVOS: any) {
    return axios({
      method: 'post',
      url: `${this.orgPrefix}/state_machine_scheme/create_config/${schemeId}/${stateMachineId}`,
      data: schemeVOS,
    });
  }

  /**
    * 根据方案id加载状态机方案
    * @param schemeId 
    * @param isDraft 
    */
  load(schemeId: number, isDraft?: boolean) {
    return axios({
      method: 'get',
      url: `${this.orgPrefix}/state_machine_scheme/query_scheme_with_config/${schemeId}`,
      params: {
        isDraft,
      },
    });
  }

  /**
    * 加载状态机方案列表
    * @param page 
    * @param size 
    * @param sort 
    * @param searchVO 
    */
  loadList(page: number = 1, size: number = 20, sort?: string, searchVO?: {
        name?: string, description?: string, param?: string
    }) {
    return axios({
      method: 'get',
      url: `${this.orgPrefix}/state_machine_scheme`,
      params: {
        page,
        size,
        sort,
        ...searchVO,
      },
    });
  }

  /**
     * 更新状态机方案
     * @param schemeId 
     * @param data 
     */
  update(schemeId: number, data: UScheme) {
    return axios({
      method: 'put',
      url: `${this.orgPrefix}/state_machine_scheme/${schemeId}`,
      data,
    });
  }

  /**
       * 删除状态机方案配置
       * @param schemeId 
       * @param stateMachineId 
       */
  deleteConfig(schemeId: number, stateMachineId: number) {
    axios({
      method: 'delete',
      url: `${this.orgPrefix}/state_machine_scheme/delete_config/${schemeId}/${stateMachineId}`,
    });
  }

  /**
       * 删除草稿
       * @param schemeId 
       */
  deleteDraft(schemeId: number) {
    return axios.delete(`${this.orgPrefix}/state_machine_scheme/delete_draft/${schemeId}`);
  }

  /**
       * 检验发布的状态机方案
       * @param schemeId 
       */
  checkPublish(schemeId: number) {
    return axios.get(`${this.orgPrefix}/state_machine_scheme/check_deploy/${schemeId}`);
  }

  checkName(name: string) {
    return axios({
      method: 'get',
      url: `${this.orgPrefix}/state_machine_scheme/check_name`,
      params: {
        name,
      },
    });
  }

  /**
       * 发布状态机方案
       * @param schemeId 
       * @param objectVersionNumber 
       * @param data 
       */
  publish(schemeId: number, objectVersionNumber: number, data: IPublishScheme) {
    return axios({
      method: 'post',
      url: `${this.orgPrefix}/state_machine_scheme/deploy/${schemeId}`,
      params: {
        objectVersionNumber,
      },
      data,
    });
  }
}

const stateMachineSchemeApi = new StateMachineSchemeApi();
export { stateMachineSchemeApi };
