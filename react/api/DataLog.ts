import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import { sameProject } from '@/utils/detail';
import Api from './Api';

class DataLogApi extends Api<DataLogApi> {
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

  /**
   *根据issueId查询操作记录
   * @param issueId
   */
  loadByIssue(issueId:number) {
    return this.isOutside ? this.request({
      method: 'get',
      url: `${this.outPrefix}/data_log`,
      params: {
        project_id: this.projectId,
        organizationId: this.orgId,
        issueId,
      },
    }) : this.request({
      method: 'get',
      url: `/agile/v1/projects/${getProjectId()}/${sameProject(this.projectId) ? '' : 'project_invoke_agile/'}data_log`,
      params: {
        issueId,
        instanceProjectId: this.projectId,
      },
    });
  }

  /**
   * 项目层下查询工作项记录 (项目群)
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
