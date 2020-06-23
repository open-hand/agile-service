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

  /**
   * 项目层下查询问题记录 (项目群)
   * @param issueId 
   * @param programId 项目群id
   */
  loadUnderProgram(issueId:number, programId:number) {
    return axios({
      method: 'get',
      url: `${this.prefix}/project_invoke_program/datalog`,
      params: {
        programId,
        issueId,
      },
    });
  }
}

const dataLogApi = new DataLogApi();
export { dataLogApi };
