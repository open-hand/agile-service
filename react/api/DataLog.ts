import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

class DataLogApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }
  
  /**
   *根据issueId查询操作记录
   * @param issueId 
   */
  loadByIssue(issueId:number) {
    return axios({
      method: 'get',
      url: `${this.prefix}/data_log`,
      params: {
        issueId,
      },
    });
  }
}

const dataLogApi = new DataLogApi();
export { dataLogApi };
