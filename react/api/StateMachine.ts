import { axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '@/utils/common';

interface IStateMachine {
    name: string,
    organizationId: number,
}
interface UStateMachine extends IStateMachine{
    objectVersionNumber:number
}
class StateMachineApi {
  get orgPrefix() {
    return `/agile/v1/organizations/${getOrganizationId()}`;
  }

  /**
     * 分页加载状态机列表
     * @param map 
     * @param sort 
     */
  loadList(map = {}, sort = { field: 'id', order: 'desc' }) {
    return axios({
      method: 'get',
      url: `${this.orgPrefix}/state_machine`,
      params: {
        ...map,
        sort: `${sort.field},${sort.order}`,
      },
    });
  }

  /**
   * 查询当前组织下所有状态机
   */
  loadAll() {
    return axios.get(`${this.orgPrefix}/state_machine/query_all`);
  }

  /**
     * 根据stateId加载状态机和配置(活跃)
     * @param stateId 
     */
  loadWithConfig(stateId: number) {
    return axios.get(`${this.orgPrefix}/state_machine/with_config_deploy/${stateId}`);
  }

  /**
     * 根据stateId加载状态机和配置(新建/草稿)
     * @param stateId 
     */
  loadWithDraftConfig(stateId: number) {
    return axios.get(`${this.orgPrefix}/state_machine/with_config_draft/${stateId}`);
  }

  /**
     * 创建状态机
     * @param data 
     */
  create(data: IStateMachine) {
    return axios({
      method: 'post',
      url: `${this.orgPrefix}/state_machine`,
      data,
    });
  }

  /**
   * 加载状态机节点 （草稿）
   * @param nodeId 
   */
  loadNode(nodeId:number) {
    return axios.get(`${this.orgPrefix}/state_machine_node/${nodeId}`);
  }

  /**
   * 编辑状态机时增加节点
   * @param stateMachineId 
   * @param map 
   */
  addNode(stateMachineId:number, node:any) {
    return axios({
      method: 'post',
      url: `${this.orgPrefix}/state_machine_node`,
      params: {
        stateMachineId,
      },
      data: node,
    });
  }


  /**
   * 更新状态机节点 （草稿）
   * @param stateMachineId 
   * @param nodeId 
   * @param node 
   */
  updateNode(stateMachineId:number, nodeId:number, node:any) {
    return axios({
      method: 'put',
      url: `${this.orgPrefix}/state_machine_node/${nodeId}`,
      params: {
        stateMachineId,
      },
      data: node,
    });
  }

  /**
   * 删除状态机节点 （草稿）
   * @param stateMachineId 
   * @param nodeId 
   */
  deleteNode(stateMachineId:number, nodeId:number) {
    return axios({
      method: 'delete',
      url: `${this.orgPrefix}/state_machine_node/${nodeId}`,
      params: {
        stateMachineId,
      },
    });
  }

  /**
   * 检查状态机节点是否可删除 (草稿)
   * @param orgId 
   * @param statusId 
   * @param stateMachineId 
   */
  checkBeforeDeleteNode(statusId:number, stateMachineId:number) {
    return axios({
      method: 'get',
      url: `${this.orgPrefix}/state_machine_node/check_delete`,
      params: {
        statusId,
        stateMachineId,
      },
    });
  }


  /**
     * 删除状态机
     * @param stateMachineId 
     */
  delete(stateMachineId: number) {
    return axios.delete(`${this.orgPrefix}/state_machine/${stateMachineId}`);
  }

  /**
     * 更新状态机
     * @param stateMachineId 
     * @param data 
     */
  update(stateMachineId: number, data: UStateMachine) {
    return axios({
      method: 'put',
      url: `${this.orgPrefix}/state_machine/${stateMachineId}`,
      data,
    });
  }

  /**
   * 发布状态机
   * @param stateMachineId 
   */
  publish(stateMachineId:number) {
    return axios.get(`${this.orgPrefix}/state_machine/deploy/${stateMachineId}`);
  }

  /**
   * 删除草稿
   * @param stateMachineId 
   */
  deleteDraft(stateMachineId:number) {
    return axios.delete(`${this.orgPrefix}/state_machine/delete_draft/${stateMachineId}`);
  }

  /**
   * 更改条件策略
   * @param transformId 
   * @param type 
   */
  updateCondition(transformId:number, type:string) {
    return axios({
      method: 'get',
      url: `${this.orgPrefix}/state_machine_transform/update_condition_strategy/${transformId}`,
      params: {
        condition_strategy: type,
      },
    });
  }

  /**
   * 创建【全部】转换，所有节点均可转换到当前节点（草稿）
   * @param endNodeId 
   * @param stateMachineId 
   */
  createAllTransferToNode(endNodeId:number, stateMachineId:number) {
    return axios({
      method: 'post',
      url: `${this.orgPrefix}/state_machine_transform/create_type_all`,
      params: {
        end_node_id: endNodeId,
        state_machine_id: stateMachineId,
      },
    });
  }

  /**
   * 删除 【全部】转换 （草稿）
   * @param transformId 
   */
  deleteAllTransferToNode(transformId:number) {
    return axios.delete(`${this.orgPrefix}/state_machine_transform/delete_type_all/${transformId}`);
  }

  /**
   * 加载状态机的转换
   * @param transferId 
   */
  loadTransfer(transferId:number) {
    return axios.get(`${this.orgPrefix}/state_machine_transform/${transferId}`);
  }

  /**
   * 获取未配置的条件，验证，后置动作等列表
   * @param transformId 
   * @param type 
   */
  loadTransferConfig(transformId:number, type:string) {
    return axios({
      method: 'get',
      url: `${this.orgPrefix}/config_code/${transformId}`,
      params: {
        type,
      },
    });
  }

  /**
   * 创建状态机转换（草稿）
   * @param stateMachineId 
   * @param data 
   */
  addTransfer(stateMachineId:number, data:any) {
    return axios({
      method: 'post',
      url: `${this.orgPrefix}/state_machine_transform`,
      params: {
        stateMachineId,
      },
      data,
    });
  }

  /**
   * 更新状态机转换 （草稿）
   * @param stateMachineId 
   * @param transferId 
   * @param data 
   */
  updateTransfer(stateMachineId:number, transferId:number, data:any) {
    return axios({
      method: 'put',
      url: `${this.orgPrefix}/state_machine_transform/${transferId}`,
      params: {
        stateMachineId,
      },
      data,
    });
  }

  /**
   * 删除状态机转换 （草稿）
   * @param stateMachineId 
   * @param transferId 
   */
  deleteTransfer(stateMachineId:number, transferId:number) {
    return axios({
      method: 'delete',
      url: `${this.orgPrefix}/state_machine_transform/${transferId}`,
      params: {
        stateMachineId,
      },
    });
  }

  /**
   * 创建配置 （草稿）
   * @param stateMachineId 
   * @param transformId 
   * @param data 
   */
  addConfig(stateMachineId:number, transformId:number, data:any) {
    return axios({
      method: 'post',
      url: `${this.orgPrefix}/state_machine_config/${stateMachineId}`,
      params: {
        transform_id: transformId,
      },
      data,
    });
  }

  /**
   * 删除配置 （草稿）
   * @param configId 
   */
  deleteConfig(configId:number) {
    return axios.delete(`${this.orgPrefix}/state_machine_config/${configId}`);
  }

  /**
    * 检查状态机名称是否重复
    * @param name 
    */
  checkName(name: string) {
    return axios({
      method: 'get',
      url: `${this.orgPrefix}/state_machine/check_name`,
      params: {
        name,
      },
    });
  }

  /**
   * 检查状态机转换名称是否重复
   * @param startNodeId 
   * @param endNodeId 
   * @param stateMachineId 
   * @param name 
   */
  checkTransferName(startNodeId:number, endNodeId:number, stateMachineId:number, name:string) {
    return axios({
      method: 'get',
      url: `${this.orgPrefix}/state_machine_transform/check_name`,
      params: {
        startNodeId,
        endNodeId,
        stateMachineId,
        name,
      },
    });
  }
}

const stateMachineApi = new StateMachineApi();
export { stateMachineApi };
