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
  transferAll: boolean
}
export interface IStatusCreateLink {
  issueTypeId: string
  statusId: string
  defaultStatus: boolean
  transferAll?: boolean
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

export interface IUpdateData {
  fieldId: string,
  fieldValueList: object[],
}

export interface ILinkage {
  parentIssueTypeCode: 'story' | 'task' | 'bug',
  parentIssueStatusSetting: string,
}

export interface IFeatureLinkage {
  issueTypeId: string
  statusId: string
  projectId: string
}
class StatusTransformApi extends Api<StatusTransformApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  loadList(issueTypeId: string, applyType?: 'program' | 'agile'): Promise<IStatusCirculation[]> {
    return this.request({
      method: 'get',
      url: `${this.prefix}/status_transform/list`,
      params: {
        applyType: applyType || getApplyType(),
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

  checkStatusDelete(statusId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/status/check_delete_status`,
      params: {
        statusId,
        applyType: getApplyType(),
      },
      noPrompt: true,
    });
  }

  deleteStatus(statusId?: string) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/status/delete_status`,
      params: {
        applyType: getApplyType(),
        statusId,
      },
      data: [],
    });
  }

  deleteStatusByIssueType(issueTypeId: string, nodeId: string) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/state_machine_node/delete`,
      params: {
        issueTypeId,
        nodeId,
        applyType: getApplyType(),
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
    issueTypeId: string, param: string, page: number = 0, size: number = 10, schemeCode: string = 'agile_issue',
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
        schemeCode,
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
        applyType: getApplyType(),
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
      url: `${this.prefix}/status_linkages/list`,
      params: {
        issueTypeId,
        statusId,
      },
    });
  }

  /**
   * 更新状态联动
   * @param issueTypeId
   * @param statusId
   * @param objectVersionNumber
   * @param data
   */
  updateLinkage(
    issueTypeId: string, statusId: string, objectVersionNumber: number, data: ILinkage[],
  ) {
    return axios({
      method: 'post',
      url: `${this.prefix}/status_linkages`,
      params: {
        issueTypeId,
        statusId,
        objectVersionNumber,
        applyType: getApplyType(),
      },
      data,
    });
  }

  /**
   * 获取特性状态联动设置
   * @param issueTypeId
   * @param statusId
   */

  getFeatureLinkage(statusId: string, issueTypeId: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/status_linkages/pro/${statusId}`,
      params: {
        parentIssueTypeId: issueTypeId,
      },
    });
  }

  /**
   * 创建更新特性的状态联动
   * @param typeCode
   * @param statusId
   * @param data
   */
  updateFeatureLinkage(statusId: string, data: IFeatureLinkage[], parentIssueTypeId:string) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/status_linkages/pro/setting_story_status`,
      params: {
        statusId,
        parentIssueTypeId,
      },
      data,
    });
  }

  /**
   * 获取设置特性状态联动时可设置的状态
   * @param data
   */
  getFeatureLinkageStatus(data: { issueTypeId: string, projectId: string }, parentIssueTypeId: string) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/status_linkages/pro/list_status`,
      params: {
        parentIssueTypeId,
      },
      data,
    });
  }

  getCustomMember(issueTypeId: string, schemeCode: string = 'agile_issue') {
    const arr = [
      { code: 'projectOwner', name: '项目所有者' },
      { code: 'reporter', name: '报告人' },
    ];
    if (getApplyType() === 'agile') {
      arr.push({ code: 'assignee', name: '经办人' });
    }
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
          return [...arr, ...data, { code: 'specifier', name: '指定人' }];
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
      params: {
        applyType: getApplyType(),
      },
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
      url: `${this.prefix}/status_field_settings/list`,
      params: {
        issueTypeId,
        statusId,
      },
    });
  }

  /**
   * 更新属性
   * @param issueTypeId
   * @param statusId
   * @param objectVersionNumber
   * @param updateData
   */
  updateField(
    issueTypeId: string, statusId: string, objectVersionNumber: number, updateData: IUpdateData[],
  ) {
    return axios({
      method: 'post',
      url: `${this.prefix}/status_field_settings`,
      data: updateData,
      params: {
        issueTypeId,
        statusId,
        objectVersionNumber,
        applyType: getApplyType(),
      },
    });
  }
}

const statusTransformApi = new StatusTransformApi();
const statusTransformApiConfig = new StatusTransformApi(true);
export { statusTransformApi, statusTransformApiConfig };
