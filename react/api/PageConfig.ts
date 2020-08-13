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
  deleteIds: string[],
}
class PageConfigApi {
  get prefixOrgOrPro() {
    return `/agile/v1/${getMenuType() === 'project' ? `projects/${getProjectId()}` : `organizations/${getOrganizationId()}`}`;
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
   * @param filedId
   * @param required
   */
  updateRequired(filedId: string, required: boolean) {
    return axios({
      method: 'post',
      url: `${this.prefixOrgOrPro}/object_scheme_field/update_required`,
      params: {
        filedId,
        required,
      },
    });
  }

  /**
   * 查询字段的rank
   */
  loadRankValue() {
    return axios({
      method: 'post',
      url: `${this.prefixOrgOrPro}/object_scheme_field/rank`,
    });
  }
}

const pageConfigApi = new PageConfigApi();
export { pageConfigApi };
