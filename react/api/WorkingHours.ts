import { getProjectId } from '@/utils/common';
import Api from './Api';

class WorkingHoursApi extends Api<WorkingHoursApi> {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  /**
    * 分页搜索查询登记工作日志issue列表
    * @param page
    * @param size
    * @param issueId
    * @param content
    */
  loadIssuesDailyLog(page: number = 1, size: number = 20, issueId?: string, content?: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/work_hours/query_issue`,
      params: {
        page,
        size,
        issueId,
        params: content,
        self: false,
      },
    });
  }
}

const workingHoursApi = new WorkingHoursApi();
const workingHoursApiConfig = new WorkingHoursApi(true);
export { workingHoursApi, workingHoursApiConfig };
