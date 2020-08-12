import { axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId, getMenuType } from '@/utils/common';

export enum PageConfigIssueType {
  feature = 'feature',
  bug = 'bug',
  subTask = 'subTask',
  story = 'story',
  task = 'task',
  epic = 'epic',
  demand = 'demand',
}
interface IFiled {
  created: true
  defaultValue: any,
  edited: true
  fieldId: string,
  fieldName: string,
  id: string,
  issueType: PageConfigIssueType,
  objectVersionNumber: number,
  rank: string,
  required: true
  source: string,
}
type FiledUpdate = Required<Pick<IFiled, 'fieldId' | 'required' | 'created' | 'edited' | 'objectVersionNumber'>>;
interface UIssueTypeConfig {
  issueType: PageConfigIssueType,
  fields: Array<FiledUpdate>,
  issueTypeFieldVO: {
    id: string,
    template: string,
    objectVersionNumber: number,
  },
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
  loadByIssueType(issueType: PageConfigIssueType) {
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
      method: 'put',
      url: `${this.prefixOrgOrPro}/object_scheme_field/configs`,
      data,
    });
  }

  /**
   * 更新字段是否必选
   * @param filedId
   * @param required
   */
  updateRequired(filedId:string, required:boolean) {
    return axios({
      method: 'post',
      url: `${this.prefixOrgOrPro}/object_scheme_field/update_required`,
      params: {
        filedId,
        required,
      },
    });
  }
}

const pageConfigApi = new PageConfigApi();
export { pageConfigApi };
