import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import Api from './Api';

interface IWorkLog {
    issueId: number|string,
    projectId: number,
    workTime: number,
    startDate: string,
    residualPrediction: string, // 剩余估计类型 'self_adjustment' 'no_set_prediction_time'  'set_to' 'reduce'
    description?: string,
    predictionTime?:number, // 预估时间
}
class WorkLogApi extends Api<WorkLogApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  /**
    * 根据issueId加载工作日志
    * @param issueId
    */
  loadByIssue(issueId: number) {
    return this.request({
      url: `${this.prefix}/work_log/issue/${issueId}`,
      method: 'get',
    });
  }

  /**
     * 创建工作日志
     * @param data
     */
  create(data: IWorkLog) {
    return this.request({
      url: `${this.prefix}/work_log`,
      method: 'post',
      data,
    });
  }

  /**
     * 更新工作日志
     * @param logId
     * @param worklog
     */
  update(logId: number, data: any) {
    return axios.patch(`${this.prefix}/work_log/${logId}`, data);
  }

  /**
     * 根据logId删除工作日志
     * @param logId
     * @param projectId
     */
  delete(logId: number) {
    return axios.delete(`${this.prefix}/work_log/${logId}`);
  }
}

const workLogApi = new WorkLogApi();
export { workLogApi };
