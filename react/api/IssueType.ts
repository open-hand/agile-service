import { axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '@/utils/common';
import { IIssueType } from '@/common/types';
import Api from './Api';

export interface ICreate {
  name: string
  icon: string
  description: string
  colour: string
}

export interface IUpdate extends ICreate {
  id: string
}
class IssueTypeApi extends Api<IssueTypeApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  get OrgPrefix() {
    return `/agile/v1/organizations/${getOrganizationId()}`;
  }

  load({ params, data }: { params: { page: number, size: number }, data: any }) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/issue_type/list`,
      params: {
        ...params,
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  create(data: ICreate) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/issue_type`,
      params: {
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  getType(typeId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/issue_type/${typeId}`,
      params: {
        organizationId: getOrganizationId(),
      },
    });
  }

  update(typeId: string, data: IUpdate) {
    return this.request({
      method: 'put',
      url: `${this.prefix}/issue_type/${typeId}`,
      params: {
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  systemUpdate(typeId: string, data: IUpdate) {
    return this.request({
      method: 'put',
      url: `${this.prefix}/issue_type/${typeId}/update_system_issue_type`,
      params: {
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  checkName(name: string, typeId?: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/issue_type/check_name`,
      params: {
        name,
        organizationId: getOrganizationId(),
        id: typeId,
      },
    });
  }

  checkIcon(icon: string, typeId?: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/issue_type/check_icon`,
      params: {
        icon,
        organizationId: getOrganizationId(),
        id: typeId,
      },
    });
  }

  getStopDisable(typeId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/issue_type/${typeId}/can_disable`,
      params: {
        organizationId: getOrganizationId(),
      },
    });
  }

  delete(typeId: string) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/issue_type/${typeId}`,
      params: {
        organizationId: getOrganizationId(),
      },
    });
  }

  orgLoad({ params, data }: { params: { page: number, size: number }, data: any }) {
    return this.request({
      method: 'post',
      url: `${this.OrgPrefix}/issue_type/list`,
      params,
      data,
    });
  }

  orghasTemplateList() {
    return this.request({
      method: 'get',
      url: `/agile/v1/organizations/${getOrganizationId()}/organization_config/issue_type/list`,
    });
  }

  orgCreate(data: ICreate) {
    return this.request({
      method: 'post',
      url: `${this.OrgPrefix}/issue_type`,
      data,
    });
  }

  orgGetType(typeId: string) {
    return this.request({
      method: 'get',
      url: `${this.OrgPrefix}/issue_type/${typeId}`,
    });
  }

  orgUpdate(typeId: string, data: IUpdate) {
    return this.request({
      method: 'put',
      url: `${this.OrgPrefix}/issue_type/${typeId}`,
      data,
    });
  }

  orgCheckName(name: string, typeId?: string) {
    return this.request({
      method: 'get',
      url: `${this.OrgPrefix}/issue_type/check_name`,
      params: {
        name,
        id: typeId,
      },
    });
  }

  orgCheckIcon(icon: string, typeId?: string) {
    return this.request({
      method: 'get',
      url: `${this.OrgPrefix}/issue_type/check_icon`,
      params: {
        icon,
        id: typeId,
      },
    });
  }

  orgDelete(typeId: string) {
    return this.request({
      method: 'delete',
      url: `${this.OrgPrefix}/issue_type/${typeId}`,
      params: {
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 加载全部工作项类型（带关联的状态机id)
   * @param applyType
   */
  loadAllWithStateMachineId(applyType: string = 'agile', projectId?: string, onlyEnabled?: boolean, programId?: string | number): Promise<IIssueType[]> {
    return this.request({
      method: 'get',
      url: `/agile/v1/projects/${projectId || getProjectId()}${programId ? '/project_invoke_program' : ''}/schemes/query_issue_types_with_sm_id`,
      params: {
        apply_type: applyType,
        only_enabled: onlyEnabled,
        programId,
      },
      // cache: true,
    });
  }

  /**
   * 加载全部工作项类型
   * @param applyType
   */
  loadAll(applyType: string = 'agile') {
    return this.request({
      method: 'get',
      url: `${this.prefix}/schemes/query_issue_types`,
      params: {
        apply_type: applyType,
      },
    });
  }

  /**
   * 根据方案id查询所有工作项类型及关联的方案
   * @param schemeId
   */
  loadAllByScheme(schemeId: number) {
    return this.request({
      method: 'get',
      url: `${this.OrgPrefix}/issue_type/query_issue_type_with_state_machine`,
      params: {
        schemeId,
      },
      // @ts-ignore
    }).then((res) => res.filter((type) => type.typeCode !== 'backlog'));
  }

  orgReferenced(typeId: string, referenced: boolean) {
    return this.request({
      method: 'put',
      url: `${this.OrgPrefix}/issue_type/${typeId}/update_referenced`,
      params: {
        typeId,
        referenced,
      },
    });
  }

  enabled(typeId: string, enabled: boolean) {
    return this.request({
      method: 'put',
      url: `${this.prefix}/issue_type/${typeId}/update_enabled`,
      params: {
        typeId,
        enabled,
        organizationId: getOrganizationId(),
      },
    });
  }

  orgGetUsage(typeId: string, page = 0, size = 15) {
    return this.request({
      method: 'get',
      url: `${this.OrgPrefix}/issue_type/${typeId}/usage_detail`,
      params: {
        page,
        size,
      },
    });
  }

  getReferencedList(page = 1, size = 20) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/issue_type/list/reference`,
      params: {
        page,
        size,
        organizationId: getOrganizationId(),
      },
    });
  }

  referenced(typeId: string, data: { name: string, copyStatusMachine:boolean} | {}) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/issue_type/reference/${typeId}`,
      params: {
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  /**
   * 工作项类型排序更新
   * @param typeId
   * @param data
   * @returns
   */
  updateRank(typeId: string, data: { frontId:string, backId?:string} | { frontId?:string, backId:string}) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/issue_type/${typeId}/update_rank`,
      params: {
        organizationId: getOrganizationId(),
      },
      data,
    });
  }
}

const issueTypeApi = new IssueTypeApi();
const issueTypeApiConfig = new IssueTypeApi(true);
export { issueTypeApi, issueTypeApiConfig };
