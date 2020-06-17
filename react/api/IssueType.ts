import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

class IssueTypeApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  /**
   * 加载全部问题类型（带关联的状态机id)
   * @param applyType 
   */
  loadAllWithStateMachineId(applyType: string = 'agile') {
    return axios({
      method: 'get',
      url: `${this.prefix}/schemes/query_issue_types_with_sm_id`,
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
}

const issueTypeApi = new IssueTypeApi();
export { issueTypeApi };
