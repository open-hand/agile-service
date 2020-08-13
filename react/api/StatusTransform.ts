import { axios } from '@choerodon/boot';
import { getProjectId, getApplyType, getOrganizationId } from '@/utils/common';
import { IStatus, IField } from '@/common/types';
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

export interface ICondition {
  type: 'specifier' | 'projectOwner',
  userIds?: string[],
}

export interface IUpdateNotifySetting {
  issueTypeId: string,
  projectId: number,
  statusId: string,
  userTypeList: string[],
  noticeTypeList: string[],
  userIdList: string[],
  objectVersionNumber: number,
}

export interface IStatusTransform {
  issueTypeId: string
  statusId: string
}
export interface ITotalStatus {
  code: string
  description: string | null
  id: string
  name: string
  type: IStatus['valueCode']
  usage: string | null
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

  listStatus(page: number, size: number) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/status/list_status`,
      params: {
        page,
        size,
      },
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

  checkStatusNeedTransform(statusId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/status/status`,
      params: {
        applyType: getApplyType(),
        statusId,
      },
    });
  }

  deleteStatus(statusId?: string, transforms: IStatusTransform[] = []) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/status/delete_status`,
      params: {
        applyType: getApplyType(),
        statusId,
      },
      data: transforms,
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

  /**
   * 获取自定义状态流转列表
   * @param issueTypeId
   * @param page
   * @param size
   */
  getCustomCirculationList(
    issueTypeId: string, param: string, page: number = 0, size: number = 10,
  ) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/status_transform_setting/list`,
      params: {
        issueTypeId,
        applyType: getApplyType(),
        page,
        size,
        param,
      },
    });
  }

  /**
   * 获取流转条件
   */
  getCondition(issueTypeId: string, statusId: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/status_transfer_setting/query_transfer`,
      params: {
        issueTypeId,
        statusId,
      },
    });
  }

  /**
   * 更新状态流转条件
   * @param issueTypeId
   * @param statusId
   * @param objectVersionNumber
   */
  updateCondition(
    issueTypeId: string, statusId: string, objectVersionNumber: number, data: ICondition[],
  ) {
    return axios({
      method: 'post',
      url: `${this.prefix}/status_transfer_setting`,
      params: {
        issueTypeId,
        statusId,
        objectVersionNumber,
      },
      data,
    });
  }

  /**
   * 获取状态联动设置
   * @param issueTypeId
   * @param statusId
   */

  getLinkage(issueTypeId: string, statusId: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/status_transform/setting_default_status`,
      params: {
        issueTypeId,
        statusId,
      },
    });
  }

  getCustomMember(issueTypeId: string, schemeCode: string = 'agile_issue') {
    const arr = [
      { id: 'projectOwner', name: '项目所有者' },
      { id: 'assignee', name: '经办人' },
      { id: 'reporter', name: '报告人' },
      { id: 'specifier', name: '指定人' },
    ];
    return this.request({
      method: 'get',
      url: `${this.prefix}/object_scheme_field/member_list`,
      params: {
        organizationId: getOrganizationId(),
        issueTypeId,
        schemeCode,
      },
      transformResponse: (res: IField) => {
        if (typeof res === 'string') {
          const data = JSON.parse(res);
          return [...arr, ...data];
        }
        return res;
      },
    });
  }

  /**
   * 获取通知设置
   * @param issueTypeId
   * @param statusId
   */
  getNotifySetting(issueTypeId: string, statusId: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/status_notice_settings/issue_type/${issueTypeId}/status/${statusId}`,
    });
  }

  updateNotifySetting(data: IUpdateNotifySetting) {
    return axios({
      method: 'post',
      url: `${this.prefix}/status_notice_settings`,
      data,
    });
  }

  /**
   * 获取更新属性信息
   * @param issueTypeId
   * @param statusId
   */
  getUpdateFieldInfo(issueTypeId: string, statusId: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/status_transform/setting_default_status`,
      params: {
        issueTypeId,
        statusId,
      },
    });
  }
}

const statusTransformApi = new StatusTransformApi();
const statusTransformApiConfig = new StatusTransformApi(true);
export { statusTransformApi, statusTransformApiConfig };
