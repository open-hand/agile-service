import { axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '@/utils/common';

interface IFiled {
  context: string, // "task"
  pageCode: string, // "agile_issue_create"
  schemeCode: string, // "agile_issue"
}
interface UIssueFiled{
  fieldType:string,
  value: any,
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
    return axios.post(`${this.prefix}/field_value/quick_create/${issueId}`, dto);
  }

  /**
 * 加载字段配置
 * @returns {V|*}
 */
  getFields(dto: IFiled) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'post',
      url: `${this.prefix}/field_value/list`,
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
  getCustomFields() {
    return axios.get(`${this.prefix}/field_value/list/custom_field`);
  }

  /**
   * 新增Issue字段值
   * @param dto 自定义字段列表
   * @returns {V|*}
   */
  createFieldValue(issueId: number, schemeCode: string, dto?: Array<any>) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'post',
      url: `${this.prefix}/field_value/${issueId}`,
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
  updateFieldValue(issueId:number, fieldId:number, schemeCode:string, dto:UIssueFiled) {
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
   * 项目层，获取自定义字段表头
   */
  getFoundationHeader() {
    return axios.get(`${this.prefix}/field_value/list/getFields`, {
      params: {
        project_id: getProjectId(),
        organizationId: getOrganizationId(),
        schemeCode: 'agile_issue',
      },
    });
  }
}

const fieldApi = new FieldApi();
export { fieldApi };
