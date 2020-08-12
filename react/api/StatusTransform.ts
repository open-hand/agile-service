import { axios } from '@choerodon/boot';
import { getProjectId, getApplyType } from '@/utils/common';
import { IStatus } from '@/common/types';
import Api from './Api';

export interface IStatusCirculation {
  code: string
  defaultStatus: boolean
  id: string
  name: string
  nodeId: string
  stateMachineId: string
  hasIssue: boolean
  type: IStatus['valueCode']
  canTransformStatus: string[];
  default?: boolean
  [propName: string]: any
}
export interface IUpdateTransform {
  startNodeId: string
  endNodeId: string
  startStatusName: string
  endStatusName: string
  select: boolean
}
export interface IStatusCreate {
  name: string
  type: IStatus['valueCode']
  defaultStatus: boolean
}
export interface IStatusCreateLink {
  issueTypeId: string
  statusId: string
  defaultStatus: boolean
}
class StatusTransformApi extends Api {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  loadList(issueTypeId: string): Promise<IStatusCirculation[]> {
    return axios({
      method: 'get',
      url: `${this.prefix}/status_transform/list`,
      params: {
        applyType: getApplyType(),
        issueTypeId,
      },
    });
  }

  setDefaultStatus(issueTypeId: string, statusId: string, stateMachineId: string) {
    return axios({
      method: 'put',
      url: `${this.prefix}/status_transform/setting_default_status`,
      params: {
        issueTypeId,
        statusId,
        stateMachineId,
      },
    });
  }

  batchUpdate(issueTypeId: string, nodes: IUpdateTransform[]) {
    return axios({
      method: 'put',
      url: `${this.prefix}/status_transform/update`,
      params: {
        issueTypeId,
        applyType: getApplyType(),
      },
      data: nodes,
    });
  }

  createStatus(issueTypeIds: string[], status: IStatusCreate) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/status/create`,
      params: {
        issueTypeIds: issueTypeIds.join(','),
        applyType: getApplyType(),
      },
      data: status,
    });
  }

  linkStatus(statusLink: IStatusCreateLink) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/state_machine/link_status`,
      params: {
        ...statusLink,
        applyType: getApplyType(),
      },
    });
  }

  deleteStatusByIssueType(issueTypeId: string, nodeId: string, toStatusId?: string) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/state_machine_node/delete`,
      params: {
        issueTypeId,
        nodeId,
        applyType: getApplyType(),
        statusId: toStatusId,
      },
    });
  }
}

const statusTransformApi = new StatusTransformApi();
const statusTransformApiConfig = new StatusTransformApi();
export { statusTransformApi, statusTransformApiConfig };
