import { axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId, getApplyType } from '@/utils/common';

interface IFiled {
  context: string, // "task"
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
class FieldApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
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
  getFields(dto: IFiled, projectId?: number) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'post',
      url: `/agile/v1/projects/${projectId || getProjectId()}/field_value/list`,
      params: {
        organizationId,
      },
      data: dto,
    });
  }

  /**
 * 加载字段配置（包含值）
 * @returns {V|*}
 */
  getFieldAndValue(issueId: number, dto: IFiled) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'post',
      url: `${this.prefix}/field_value/list/${issueId}`,
      params: {
        organizationId,
      },
      data: dto,
    });
  }

  /**
   * 获取项目下自定义的字段
   */
  getCustomFields(issueType?: 'agileIssueType' | 'programIssueType') {
    return axios({
      method: 'get',
      url: `${this.prefix}/field_value/list/custom_field`,
      params: {
        issueType: issueType ?? getApplyType() === 'program' ? 'programIssueType' : 'agileIssueType',
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
  updateFieldValue(issueId: number, fieldId: number, schemeCode: string, dto: UIssueFiled) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'post',
      url: `${this.prefix}/field_value/update/${issueId}`,
      params: {
        organizationId,
        schemeCode,
        fieldId,
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
  getFoundationHeader(issueType?: 'agileIssueType' | 'programIssueType') {
    return axios.get(`${this.prefix}/field_value/list/getFields`, {
      params: {
        project_id: getProjectId(),
        organizationId: getOrganizationId(),
        schemeCode: 'agile_issue',
        issueType: issueType ?? getApplyType() === 'program' ? 'programIssueType' : 'agileIssueType',
      },
    });
  }
}

const fieldApi = new FieldApi();
export { fieldApi };
