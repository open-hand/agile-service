import { axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId, getApplyType } from '@/utils/common';
import { sameProject } from '@/utils/detail';
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
class FieldApi extends Api<FieldApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  get outPrefix() {
    return '/agile/v1/backlog_external';
  }

  get isOutside() {
    return false;
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

    return axios({
      method: 'post',
      url: `${this.prefix}/field_value/quick_create/${issueId}`,
      data: dto,
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
    return axios({
      method: 'post',
      url: `/agile/v1/projects/${projectId || getProjectId()}/field_value/list`,
      params: {
        organizationId: this.orgId,
      },
      data: dto,
    });
  }

  /**
 * 加载字段配置（包含值）
 * @returns {V|*}
 */
  getFieldAndValue(issueId: string, dto: IFiled) {
    return this.isOutside ? this.request({
      method: 'post',
      url: `${this.outPrefix}/field_value/list/${issueId}`,
      params: {
        projectId: this.projectId,
        organizationId: this.orgId,
      },
      data: dto,
    }) : this.request({
      method: 'post',
      url: `/agile/v1/projects/${getProjectId()}/${sameProject(this.projectId) ? '' : 'project_invoke_agile/'}field_value/list/${issueId}`,
      params: {
        organizationId: this.orgId,
        instanceProjectId: this.projectId,
      },
      data: dto,
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
  createFieldValue(issueId: number, schemeCode: string, dto?: Array<any>, projectId?: number) {
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
   * 批量更新问题字段
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
    return axios.get(`${this.prefix}/field_value/list/getFields`, {
      params: {
        project_id: getProjectId(),
        organizationId: getOrganizationId(),
        schemeCode: 'agile_issue',
        issueTypeList: issueTypeList ?? getApplyType() === 'program' ? 'programIssueType' : 'agileIssueType',
      },
    });
  }
}

const fieldApi = new FieldApi();
export { fieldApi };
