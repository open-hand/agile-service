import { axios } from '@choerodon/boot';
import { IFieldType } from '@/common/types';
import { getProjectId, getOrganizationId, getMenuType } from '@/utils/common';
import { sameProject } from '@/utils/detail';
import Api from './Api';
import { IImportOrExportRecord } from './Issue';

export enum PageConfigIssueType {
  feature = 'feature',
  bug = 'bug',
  subTask = 'subTask',
  story = 'story',
  task = 'task',
  epic = 'issue_epic',
  demand = 'demand',
  null = '',
}
interface ISyncDefaultPostData {
  defaultValue?: any
  extraConfig?: boolean
  fieldOptions?: Array<IFieldOption & { isDefault: boolean }>
  custom: boolean
  fieldType?: IFieldType
  issueTypeIds: string[]
}
export interface IFieldOption {
  id: string,
  fieldId: string,
  code: string,
  value: string,
  tempKey?: string,
}
export type IFieldOptionProps = IFieldOption;
interface IFiled {
  created: boolean,
  defaultValue: any,
  edited: boolean,
  fieldId: string,
  fieldCode: string,
  fieldName: string,
  fieldType: string,
  defaultValueObj: any,
  defaultValueObjs?: Array<any>,
  fieldOptions: Array<IFieldOption> | null
  id: string,
  issueType: PageConfigIssueType,
  objectVersionNumber: number,
  rank: string,
  required: boolean,
  source: string,
  pageConfigFieldEdited: any
}

interface IFiledListItem {
  code: string,
  context: 'feature' | 'bug' | 'sub_task' | 'issue_epic' | 'task' | 'story' | 'backlog',
  contextName: string,
  defaultValue: any,
  defaultValueObj: Array<any> | null,
  fieldOptions: any,
  description: string,
  fieldType: string,
  fieldTypeName: string,
  id: string,
  name: string,
  objectVersionNumber: number,
  organizationId: string,
  projectId: string | null,
  requiredScope: 'ALL' | 'PART' | 'NONE',
  system: boolean,
}
export type IFiledListItemProps = IFiledListItem;
export type IFiledProps = IFiled;
interface IssueTypeFieldVO {
  id: string,
  template: string,
  objectVersionNumber: number,
}
interface PageIssueType {
  fields: IFiled[],
  issueTypeFieldVO?: IssueTypeFieldVO,
}
type FiledUpdate = Required<Pick<IFiled, 'fieldId' | 'required' | 'created' | 'edited' | 'objectVersionNumber'>>;
export interface UIssueTypeConfig {
  issueTypeId: string,
  fields: Array<FiledUpdate>,
  issueTypeFieldVO?: Partial<IssueTypeFieldVO>,
  deleteIds?: string[],
  addIds?: Array<{
    fieldId: string,
    rank: string
  }>,
  createdFields?: Array<any>,

}
export interface ICascadeRule {
  issueTypeId: string,
  fieldId: string,
  fieldOptionId: string,
  cascadeFieldId: string,
  defaultValue?: any,
  hidden?: boolean,
  required?: boolean,
  fieldCascadeRuleOptionList?: Array<{
    cascadeOptionId: string,
    defaultOption?: boolean
  }> | Array<{
    projectId: string,
    defaultOption?: boolean
  }>
  _status: 'create' | 'update' | 'delete'
  objectVersionNumber?: number
  id?: number
}

type IPageFieldCreatePermissionScope = 'read' | 'write'
export interface IPageFieldCreatePermissionItem {
  scope: IPageFieldCreatePermissionScope
  userIds?: string[]
  roleIds?: string[]
}
export interface IPageFieldCreatePermissionRoleItem {
  scope: IPageFieldCreatePermissionScope
  roleIds: string[]
}
interface IPageFieldCreatePermission {
  fields: Array<{ id: string, code: string }>
  permissions: Array<IPageFieldCreatePermissionItem>
  issueTypeIds: string[]
}

export interface IPageFieldPermissionItem {
  roleIds: string[]
  roleList: null
  scope: IPageFieldCreatePermissionScope
  userList: null
  userIds: string[]
}

class PageConfigApi extends Api<PageConfigApi> {
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

  get prefixOrgOrPro() {
    return `/agile/v1/${getMenuType() === 'project' ? `projects/${this.projectId}` : `organizations/${this.projectId}`}`;
  }

  /**
   * 加载字段列表
   * @param schemeCode
   */
  load(schemeCode: string = 'agile_issue') {
    return this.request({
      method: 'get',
      url: `${this.prefixOrgOrPro}/object_scheme_field/list`,
      params: {
        schemeCode,
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 通过字段id加载字段详情
   * @param fieldId
   */
  loadById(fieldId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefixOrgOrPro}/object_scheme_field/${fieldId}`,
      params: {
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 根据工作项类型id查询字段
   * @param issueTypeId
   */
  loadFieldsByType(issueTypeId: string) {
    return axios({
      method: 'get',
      url: `${this.prefixOrgOrPro}/field_value/list/fields`,
      params: {
        issueTypeId,
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 根据工作项类型code查询默认模板
   * @param issueType
   */
  loadTemplateByType(issueType: string, projectId?: string) {
    return this.request({
      method: 'get',
      url: `${projectId ? `/agile/v1/projects/${projectId}` : this.prefixOrgOrPro}/object_scheme_field/description_template`,
      params: {
        issueTypeId: issueType,
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 查询当前项目或组织下可配置的工作项类型
   *
   */
  loadAvailableIssueType(): Promise<{ id: string, name: string, typeCode: string }[]> {
    return this.request({
      method: 'get',
      url: `${this.prefixOrgOrPro}/object_scheme_field/configs/issue_types`,
      params: {
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 根据工作项类型加载页面配置
   * @param issueTypeId
   */
  loadByIssueType(issueTypeId: string): Promise<PageIssueType> {
    return axios({
      method: 'get',
      url: `${this.prefixOrgOrPro}/object_scheme_field/configs`,
      params: {
        issueTypeId,
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 根据工作项类型加载页面模板
   * @param issueTypeId
   */
  loadTemplateByIssueType(issueTypeId: string): Promise<PageIssueType> {
    return axios({
      method: 'get',
      url: `${this.prefixOrgOrPro}/object_scheme_field/page_template`,
      params: {
        issueTypeId,
        organizationId: getOrganizationId(),
      },
    });
  }

  updateField(fieldId: string, data: any) {
    return axios({
      method: 'put',
      url: `${this.prefixOrgOrPro}/object_scheme_field/${fieldId}`,
      data,
      params: {
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 更新页面配置
   * @param data
   */
  updateConfig(data: UIssueTypeConfig) {
    return axios({
      method: 'post',
      url: `${this.prefixOrgOrPro}/object_scheme_field/configs`,
      data,
      params: {
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 更新字段是否必选
   * @param fieldId
   * @param required
   */
  updateRequired(fieldId: string, required: boolean) {
    return axios({
      method: 'post',
      url: `${this.prefixOrgOrPro}/object_scheme_field/update_required`,
      params: {
        fieldId,
        required,
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 同步主默认值到 issueType 类型
   * @param fieldId
   * @param issueTypeStr
   */
  syncDefaultValue(fieldId: string, data: ISyncDefaultPostData) {
    return axios({
      method: 'post',
      url: `${this.prefixOrgOrPro}/object_scheme_field/sync_default_value`,
      params: {
        field_id: fieldId,
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  /**
   * 删除字段
   * @param fieldId
   */
  delete(fieldId: string) {
    return this.request({
      method: 'delete',
      url: `${this.prefixOrgOrPro}/object_scheme_field/${fieldId}`,
      params: {
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 查询当前类型未选择的字段列表
   * @param issueType
   */
  loadUnSelected(issueTypeId: string): Promise<IFiledListItemProps[]> {
    return axios({
      method: 'get',
      url: `${this.prefixOrgOrPro}/object_scheme_field/unselected`,
      params: {
        issueTypeId,
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 查询字段的rank
   */
  loadRankValue(data: {
    previousRank: string | null, nextRank: string | null,
  }) {
    return axios({
      method: 'post',
      url: `${this.prefixOrgOrPro}/object_scheme_field/rank`,
      params: {
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  /**
   * 下载导入模板
   */
  downloadTemplate() {
    return axios({
      method: 'get',
      responseType: 'arraybuffer',
      url: `${this.prefixOrgOrPro}/excel/object_scheme_field/download`,
      params: {
        organizationId: getOrganizationId(),
      },
    });
  }

  import(data: any) {
    return axios({
      method: 'post',
      headers: { 'Content-Type': 'multipart/form-data' },
      url: `${this.prefixOrgOrPro}/excel/object_scheme_field/import`,
      params: {
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  importCancel(data: { id: string, objectVersionNumber: number }) {
    return axios({
      method: 'put',
      url: `${this.prefixOrgOrPro}/excel/cancel`,
      params: {
        organizationId: getOrganizationId(),
        ...data,
      },
    });
  }

  /**
   * 查询最近导入记录
   */
  loadLastImportRecord(action = 'upload_file_customer_field'): Promise<IImportOrExportRecord> {
    return axios({
      method: 'get',
      url: `${this.prefixOrgOrPro}/excel/latest`,
      params: {
        organizationId: getOrganizationId(),
        action,
      },
    });
  }

  /**
   * 创建级联规则
   */
  createCascadeRule(data: ICascadeRule[]) {
    return axios({
      method: 'post',
      url: `${this.prefix}/field_cascade_rule`,
      data,
    });
  }

  getCascadeRuleList(issueTypeId: string, fieldId?: string) {
    return this.request({
      method: 'get',
      url: this.isOutside ? `${this.outPrefix}/field_cascade_rule` : `${this.prefix}/field_cascade_rule`,
      params: {
        issue_type_id: issueTypeId,
        field_id: fieldId,
        projectId: this.projectId,
      },
    });
  }

  getCascadeFields(issueTypeId: string, fieldId: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/field_cascade_rule/no_loop_filed`,
      params: {
        issue_type_id: issueTypeId,
        field_id: fieldId,
      },
    });
  }

  getCascadeRelOptionList(ruleId: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/field_cascade_rule/${ruleId}/option`,
    });
  }

  /**
   * 创建权限
   * @param data
   * @returns
   */
  createPermission(data: IPageFieldCreatePermission) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/field_permission/create`,
      params: {
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  /**
     * 批量创建权限
     * @param data
     * @returns
     */
  batchCreatePermission(data: IPageFieldCreatePermission) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/field_permission/batch_create`,
      params: {
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  /**
     * 根据字段id查询权限
     * @param data
     * @returns
     */
  loadFieldPermission(field: string, issueTypeId: string): Promise<IPageFieldPermissionItem[]> {
    return this.request({
      method: 'get',
      url: `${this.prefix}/field_permission/filed_id/${field}`,
      params: {
        organizationId: getOrganizationId(),
        issueTypeId,
      },
    });
  }

  /**
   * 查询必填字段是否有查看的权限
   * @param data  @default pageCode 默认为 agile_issue_create  非创建时需置为空
   * @param issueId
   * @returns
   */
  loadRequiredPermission(data: { issueTypeId: string, pageCode?: string }, issueId?: string) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/field_permission/required_fields/no_permission`,
      params: {
        organizationId: getOrganizationId(),
        issueId,
      },
      data: {
        pageCode: 'agile_issue_create',
        schemeCode: 'agile_issue',
        ...data,
      },
    });
  }
}

const pageConfigApi = new PageConfigApi();
const pageConfigApiConfig = new PageConfigApi(true);
export { pageConfigApi, pageConfigApiConfig };
