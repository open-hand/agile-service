import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import { getOrganizationId } from '@/utils/common';

class IssueTypeApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  get OrgPrefix() {
    return `/agile/v1/organizations/${getOrganizationId()}`;
  }

  /**
   * 加载全部问题类型（带关联的状态机id)
   * @param applyType 
   */
  loadAllWithStateMachineId(applyType: string = 'agile', projectId?: number) {
    return axios({
      method: 'get',
      url: `/agile/v1/projects/${projectId || getProjectId()}/schemes/query_issue_types_with_sm_id`,
      params: {
        apply_type: applyType,
      },
    });
  }

  /**
   * 加载全部问题类型
   * @param applyType 
   */
  loadAll(applyType:string = 'agile') {
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
  loadAllByScheme(schemeId:number) {
    return axios({
      method: 'get',
      url: `${this.OrgPrefix}/issue_type/query_issue_type_with_state_machine`,
      params: {
        schemeId,
      },
    });
  }
}

const issueTypeApi = new IssueTypeApi();
export { issueTypeApi };
