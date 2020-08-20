import { axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId, getMenuType } from '@/utils/common';

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
interface IFiled {
  created: boolean,
  defaultValue: any,
  edited: boolean,
  fieldId: string,
  fieldName: string,
  id: string,
  issueType: PageConfigIssueType,
  objectVersionNumber: number,
  rank: string,
  required: boolean,
  source: string,
}
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
  issueType: PageConfigIssueType,
  fields: Array<FiledUpdate>,
  issueTypeFieldVO?: Partial<IssueTypeFieldVO>,
  deleteIds?: string[],
  addIds?: string[],
  createdFields?: Array<any>,

}
class PageConfigApi {
  get prefixOrgOrPro() {
    return `/agile/v1/${getMenuType() === 'project' ? `projects/${getProjectId()}` : `organizations/${getOrganizationId()}`}`;
  }

  /**
   * 加载字段列表
   * @param schemeCode
   */
  load(schemeCode: string = 'agile_issue') {
    return axios({
      method: 'get',
      url: `${this.prefixOrgOrPro}/object_scheme_field/list`,
      params: {
        schemeCode,
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
   * 根据问题类型id查询默认模版
   * @param issueTypeId
   */
  loadTemplateByType(issueTypeId: string) {
    return axios({
      method: 'get',
      url: `${this.prefixOrgOrPro}/object_scheme_field/description_template`,
      params: {
        issueTypeId,
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
        issueType,
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 更新页面配置
   * @param data
   */
  update(data: UIssueTypeConfig) {
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
   * 查询当前类型未选择的字段列表
   * @param issueType
   */
  loadUnSelected(issueType: PageConfigIssueType): Promise<{ id: string }[]> {
    return axios({
      method: 'get',
      url: `${this.prefixOrgOrPro}/object_scheme_field/unselected`,
      params: {
        issueType,
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 查询字段的rank
   */
  loadRankValue(data: {
    before: boolean, issueType: PageConfigIssueType,
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
}

const pageConfigApi = new PageConfigApi();
export { pageConfigApi };
