import { axios } from '@choerodon/boot';
import { castArray } from 'lodash';
import {
  getProjectId, getOrganizationId, getApplyType, getMenuType,
} from '@/utils/common';
import { sameProject } from '@/utils/detail';
import { FieldOption, IField, IssueCreateFields } from '@/common/types';
import Api from './Api';

interface IFiled {
  issueTypeId: string, // "task"
  pageCode: string, // "agile_issue_create"
  schemeCode: string, // "agile_issue"
}
interface UIssueFiled {
  fieldType: string,
  value: any,
}
interface BathUpdateCustom extends UIssueFiled {
  fieldId: number,
}
interface BathUpdateField {
  customFields: Array<BathUpdateCustom>,
  issueIds: Array<number>,
  predefinedFields: Array<any>,
}

export interface FieldOptionCreate {
  code: string
  value: string
  enabled: boolean
  sequence: number
}
class FieldApi extends Api<FieldApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  get prefixOrgOrPro() {
    return `/agile/v1/${getMenuType() === 'project' ? `projects/${this.projectId}` : `organizations/${this.projectId}`}`;
  }

  get outPrefix() {
    return '/agile/v1/backlog_external';
  }

  get isOutside() {
    return false;
  }

  /** 自定义选项接口前缀 */
  get prefixCustomOption() {
    if (this.isOutside) {
      return `${this.outPrefix}/field_value`;
    }
    return `/agile/v1/${getMenuType() === 'project' ? `projects/${this.projectId}/field_value` : `organizations/${this.orgId}/object_scheme_field`}`;
  }

  outside(outside: boolean) {
    return this.overwrite('isOutside', outside);
  }

  /**
     * 快速创建字段默认值
     * @param issueId
     * @param dto
     */
  quickCreateDefault(issueId: number, dto: IFiled) {
    const organizationId = getOrganizationId();

    return this.request({
      method: 'post',
      url: `${this.prefix}/field_value/quick_create/${issueId}`,
      data: { ...dto },
      params: {
        organizationId,
      },
    });
  }

  /**
 * 加载字段配置
 * @returns {V|*}
 */
  getFields(dto: IFiled, projectId?: string) {
    return this.request({
      method: 'post',
      url: `/agile/v1/projects/${projectId || getProjectId()}/field_value/list`,
      params: {
        organizationId: this.orgId,
      },
      data: { ...dto },
    });
  }

  /**
 * 加载字段配置（包含值）
 * @returns {V|*}
 */
  getFieldAndValue(issueId: string, dto: IFiled, projectId?: string) {
    return this.isOutside ? this.request({
      method: 'post',
      url: `${this.outPrefix}/field_value/list/${issueId}`,
      params: {
        projectId: this.projectId,
        organizationId: this.orgId,
      },
      data: { ...dto },
    }) : this.request({
      method: 'post',
      url: `/agile/v1/projects/${projectId || getProjectId()}/${sameProject(this.projectId) ? '' : 'project_invoke_agile/'}field_value/list/${issueId}`,
      params: {
        organizationId: this.orgId,
        instanceProjectId: this.projectId,
      },
      data: { ...dto },
    });
  }

  /**
   * 获取项目下自定义的字段
   */
  getCustomFields(issueTypeList?: 'agileIssueType' | 'programIssueType') {
    return axios({
      method: 'get',
      url: `${this.prefix}/field_value/list/custom_field`,
      params: {
        issueTypeList: issueTypeList ?? getApplyType() === 'program' ? 'programIssueType' : 'agileIssueType',
      },
    });
  }

  /**
   * 新增Issue字段值
   * @param dto 自定义字段列表
   * @returns {V|*}
   */
  createFieldValue(issueId: number, schemeCode: string, dto?: Array<any>, projectId?: string) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'post',
      url: `/agile/v1/projects/${projectId || getProjectId()}/field_value/${issueId}`,
      params: {
        organizationId,
        schemeCode,
      },
      data: dto,
    });
  }

  /**
   * 更新Issue字段值
   * @param issueId
   * @param fieldId
   * @param schemeCode
   * @param dto  更新的值
   */
  updateFieldValue(issueId: number, fieldId: number, fieldCode: string, schemeCode: string, dto: UIssueFiled) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'post',
      url: `${this.prefix}/field_value/update/${issueId}`,
      params: {
        organizationId,
        schemeCode,
        fieldId,
        fieldCode,
      },
      data: dto,
    });
  }

  /**
   * 批量更新工作项字段
   * @param data
   * @param schemeCode
   * @param applyType
   */
  batchUpdateIssue(data: BathUpdateField, schemeCode = 'agile_issue', applyType = 'agile') {
    return axios({
      method: 'post',
      url: `${this.prefix}/field_value/batch_update_fields_value`,
      params: {
        schemeCode,
        applyType,
      },
      data,
    });
  }

  /**
   * 项目层，获取自定义字段表头
   */
  getFoundationHeader(issueTypeList?: 'agileIssueType' | 'programIssueType') {
    return this.request({
      url: `${this.prefix}/field_value/list/getFields`,
      method: 'get',
      params: {
        project_id: getProjectId(),
        organizationId: getOrganizationId(),
        schemeCode: 'agile_issue',
        issueTypeList: issueTypeList ?? getApplyType() === 'program' ? 'programIssueType' : 'agileIssueType',
      },
    });
  }

  /**
   * 项目层，获取自定义字段表头
   */
  getTableFields(issueTypeList?: 'agileIssueType' | 'programIssueType') {
    return axios.get(`${this.prefix}/field_value/list/get_fields`, {
      params: {
        project_id: getProjectId(),
        organizationId: getOrganizationId(),
        schemeCode: 'agile_issue',
        issueTypeList: issueTypeList ?? getApplyType() === 'program' ? 'programIssueType' : 'agileIssueType',
      },
    }).then((res: IField[]) => {
      const issueNum = {
        code: 'issueNum',
      };
      // @ts-ignore
      res.splice(1, 0, issueNum);

      return res;
    });
  }

  /**
   *获取概要默认值
   * @param issueTypeId
   */
  getSummaryDefaultValue(issueTypeId: string): Promise<string | undefined> {
    return this.request({
      url: `${this.prefix}/field_value/summary_default_value`,
      method: 'get',
      params: {
        issueTypeId,
        organizationId: getOrganizationId(),
      },
    });
  }

  getFieldOptions(fieldId: string, searchValue: string | undefined = '', page: number | undefined, size: number, selected?: string | string[], onlyEnabled = true) {
    return axios({
      method: 'get',
      url: `${this.prefixCustomOption}/${fieldId}/options`,
      params: {
        searchValue,
        page,
        size,
        organizationId: this.orgId,
        selected: selected ? castArray(selected).join(',') || undefined : undefined,
        enabled: onlyEnabled,
      },
    });
  }

  updateFieldOption(fieldId: string, optionId: string, data: FieldOption) {
    return axios({
      method: 'put',
      url: `${this.prefixOrgOrPro}/object_scheme_field/${fieldId}/options/${optionId}`,
      params: {
        organizationId: this.orgId,
      },
      data,
    });
  }

  createFieldOption(fieldId: string, data: FieldOptionCreate) {
    return axios({
      method: 'post',
      url: `${this.prefixOrgOrPro}/object_scheme_field/${fieldId}/options`,
      params: {
        organizationId: this.orgId,
      },
      data,
    });
  }

  deleteFieldOption(fieldId: string, optionId: string) {
    return axios({
      method: 'delete',
      url: `${this.prefixOrgOrPro}/object_scheme_field/${fieldId}/options/${optionId}`,
      params: {
        organizationId: this.orgId,
      },
    });
  }

  getCascadeOptions(fieldId: string, selected: string[], fieldCascadeRuleIds: string[], searchParam: string, page: number, size: number, extendParams?: string[]) {
    return axios({
      method: 'post',
      url: `${this.prefix}/field_cascade_rule/cascade_field/${fieldId}/option`,
      params: {
        page,
        size,
      },
      data: {
        selected,
        fieldCascadeRuleIds,
        searchParam,
        extendParams,
      },
    });
  }
}
const fieldApi = new FieldApi();
export default FieldApi;
export { fieldApi };
