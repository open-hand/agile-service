import { axios } from '@choerodon/boot';
import { AxiosRequestConfig } from 'axios';
import { IFieldType } from '@/common/types';
import { getProjectId, getOrganizationId, getMenuType } from '@/utils/common';
import Api from './Api';

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
}

interface IFiledListItem {
  code: string,
  context: 'feature' | 'bug' | 'sub_task' | 'issue_epic' | 'task' | 'story' | 'backlog',
  contextName: string,
  defaultValue: any,
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
  issueTypeId: PageConfigIssueType,
  fields: Array<FiledUpdate>,
  issueTypeFieldVO?: Partial<IssueTypeFieldVO>,
  deleteIds?: string[],
  addIds?: Array<{
    fieldId: string,
    rank: string
  }>,
  createdFields?: Array<any>,

}
class PageConfigApi extends Api<PageConfigApi> {
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
   * 根据问题类型id查询字段
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
   * 根据问题类型code查询默认模版
   * @param issueType
   */
  loadTemplateByType(issueType: string) {
    return this.request({
      method: 'get',
      url: `${this.prefixOrgOrPro}/object_scheme_field/description_template`,
      params: {
        issueTypeId: issueType,
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 查询当前项目或组织下可配置的问题类型
   *
   */
  loadAvailableIssueType(): Promise<{ id: string, name: string, typeCode: string }[]> {
    return axios({
      method: 'get',
      url: `${this.prefixOrgOrPro}/object_scheme_field/configs/issue_types`,
      params: {
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 根据问题类型加载页面配置
   * @param issueType
   */
  loadByIssueType(issueType: PageConfigIssueType): Promise<PageIssueType> {
    return axios({
      method: 'get',
      url: `${this.prefixOrgOrPro}/object_scheme_field/configs`,
      params: {
        issueTypeId: issueType,
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
  loadUnSelected(issueType: PageConfigIssueType): Promise<IFiledListItemProps[]> {
    return axios({
      method: 'get',
      url: `${this.prefixOrgOrPro}/object_scheme_field/unselected`,
      params: {
        issueTypeId: issueType,
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

  loadTypesOnCreate() {
    return this.request({
      method: 'get',
      url: `${this.prefixOrgOrPro}/object_scheme_field/configs/issue_types`,
      params: {
        organizationId: getOrganizationId(),

      },
    });
  }
}

const pageConfigApi = new PageConfigApi();
const pageConfigApiConfig = new PageConfigApi(true);
export { pageConfigApi, pageConfigApiConfig };
