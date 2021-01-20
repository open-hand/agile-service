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

  load({ params, data }: { params: { page: number, size: number}, data: any}) {
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
    return axios({
      method: 'get',
      url: `${this.prefix}/issue_type/${typeId}`,
      params: {
        organizationId: getOrganizationId(),
      },
    });
  }

  update(typeId: string, data: IUpdate) {
    return axios({
      method: 'put',
      url: `${this.prefix}/issue_type/${typeId}`,
      params: {
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  checkName(name: string, typeId?: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/issue_type/check_name`,
      params: {
        name,
        organizationId: getOrganizationId(),
        id: typeId,
      },
    });
  }

  getDeleteDisable(typeId: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/issue_type/${typeId}/can_disable`,
      params: {
        organizationId: getOrganizationId(),
      },
    });
  }

  delete(typeId: string) {
    return axios({
      method: 'delete',
      url: `${this.prefix}/issue_type/${typeId}`,
      params: {
        organizationId: getOrganizationId(),
      },
    });
  }

  orgLoad({ params, data }: { params: { page: number, size: number}, data: any}) {
    return this.request({
      method: 'post',
      url: `${this.OrgPrefix}/issue_type/list`,
      params,
      data,
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
    return axios({
      method: 'get',
      url: `${this.OrgPrefix}/issue_type/${typeId}`,
    });
  }

  orgUpdate(typeId: string, data: IUpdate) {
    return axios({
      method: 'put',
      url: `${this.OrgPrefix}/issue_type/${typeId}`,
      data,
    });
  }

  orgCheckName(name: string, typeId?: string) {
    return axios({
      method: 'get',
      url: `${this.OrgPrefix}/issue_type/check_name`,
      params: {
        name,
        id: typeId,
      },
    });
  }

  orgGetDeleteDisable(typeId: string) {
    return axios({
      method: 'get',
      url: `${this.OrgPrefix}/issue_type/${typeId}/can_disable`,
    });
  }

  orgDelete(typeId: string) {
    return axios({
      method: 'delete',
      url: `${this.OrgPrefix}/issue_type/${typeId}`,
      params: {
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 加载全部问题类型（带关联的状态机id)
   * @param applyType
   */
  loadAllWithStateMachineId(applyType: string = 'agile', projectId?: string): Promise<IIssueType[]> {
    return this.request({
      method: 'get',
      url: `/agile/v1/projects/${projectId || getProjectId()}/schemes/query_issue_types_with_sm_id`,
      params: {
        apply_type: applyType,
      },
      // cache: true,
    });
  }

  /**
   * 加载全部问题类型
   * @param applyType
   */
  loadAll(applyType: string = 'agile') {
    return axios({
      method: 'get',
      url: `${this.prefix}/schemes/query_issue_types`,
      params: {
        apply_type: applyType,
      },
    });
  }

  /**
   * 根据方案id查询所有问题类型及关联的方案
   * @param schemeId
   */
  loadAllByScheme(schemeId: number) {
    return axios({
      method: 'get',
      url: `${this.OrgPrefix}/issue_type/query_issue_type_with_state_machine`,
      params: {
        schemeId,
      },
      // @ts-ignore
    }).then((res) => res.filter((type) => type.typeCode !== 'backlog'));
  }

  referenced(typeId: string) {
    return axios({
      method: 'post',
      url: `${this.OrgPrefix}/issue_type/query_issue_type_with_state_machine`,
      params: {
        typeId,
      },
    });
  }

  dontReferenced(typeId: string) {
    return axios({
      method: 'post',
      url: `${this.OrgPrefix}/issue_type/query_issue_type_with_state_machine`,
      params: {
        typeId,
      },
    });
  }

  start(typeId: string) {
    return axios({
      method: 'post',
      url: `${this.OrgPrefix}/issue_type/query_issue_type_with_state_machine`,
      params: {
        typeId,
      },
    });
  }

  stop(typeId: string) {
    return axios({
      method: 'post',
      url: `${this.OrgPrefix}/issue_type/query_issue_type_with_state_machine`,
      params: {
        typeId,
      },
    });
  }

  getUsage(page = 0, size = 15) {
    return axios({
      method: 'get',
      url: `${this.OrgPrefix}/issue_type/query_issue_type_with_state_machine`,
      param: {
        page,
        size,
      },
    });
  }
}

const issueTypeApi = new IssueTypeApi();
const issueTypeApiConfig = new IssueTypeApi(true);
export { issueTypeApi, issueTypeApiConfig };
