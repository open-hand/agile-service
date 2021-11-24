import { getIsOrganization, getOrganizationId, getProjectId } from '@/utils/common';
import Api from './Api';

export interface IWorkingHoursData {
  userIds?: string[],
  projectIds?: string[],
  startTime: string,
  endTime: string,
  workGroupIds?: string[]
}

export type WorkingHoursExportAction = 'download_file_work_hours_log' | 'download_file_work_hours_calendar'
class WorkingHoursApi extends Api<WorkingHoursApi> {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  get orgPrefix() {
    return `/agile/v1/organizations/${getOrganizationId()}`;
  }

  get workBenchPrefix() {
    return `/agile/v1/organizations/${this.orgId}/work_bench`;
  }

  getLogs(params: any, data: IWorkingHoursData) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/work_hours/work_hours_log`,
      params: {
        ...(params || {}),
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  orgGetLogs(params: any, data: IWorkingHoursData) {
    return this.request({
      method: 'post',
      url: `${this.workBenchPrefix}/work_hours/work_hours_log`,
      params: {
        ...(params || {}),
      },
      data,
    });
  }

  exportLog(data: IWorkingHoursData) {
    return this.request({
      method: 'post',
      url: `${getIsOrganization() ? this.workBenchPrefix : this.prefix}/work_hours/export_work_hours_log`,
      params: {
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  getLatest(action: WorkingHoursExportAction) {
    return this.request({
      method: 'get',
      url: `${getIsOrganization() ? this.workBenchPrefix : this.prefix}/excel/latest`,
      params: {
        organizationId: getOrganizationId(),
        action,
      },
    });
  }

  getCalendar({ params, data }:{params?: { page: number, size: number }, data: IWorkingHoursData}) {
    return this.request({
      method: 'post',
      url: `${getIsOrganization() ? this.workBenchPrefix : this.prefix}/work_hours/work_hours_calendar`,
      params: {
        organizationId: getOrganizationId(),
        ...params,
      },
      data,
    });
  }

  getCount(data: IWorkingHoursData) {
    return this.request({
      method: 'post',
      url: `${getIsOrganization() ? this.workBenchPrefix : this.prefix}/work_hours/count_work_hours`,
      params: {
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  getUserCalendar(userId: string, data: IWorkingHoursData) {
    return this.request({
      method: 'post',
      url: `${getIsOrganization() ? this.workBenchPrefix : this.prefix}/work_hours/work_hours_calendar_info`,
      params: {
        organizationId: getOrganizationId(),
        userId,
      },
      data,
    });
  }

  exportCalendar(data: IWorkingHoursData) {
    return this.request({
      method: 'post',
      url: `${getIsOrganization() ? this.workBenchPrefix : this.prefix}/work_hours/export_work_hours_calendar`,
      params: {
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  getTotalWorkTime(data: any, containsSubIssue: boolean) {
    return this.request({
      method: 'post',
      url: `${getIsOrganization() ? this.workBenchPrefix : this.prefix}/work_hours/count_issue_work_hours`,
      params: {
        organizationId: getOrganizationId(),
        containsSubIssue,
      },
      data,
    });
  }

  exportHours(data:any) {
    return this.request({
      method: 'post',
      url: `${getIsOrganization() ? this.workBenchPrefix : this.prefix}/work_hours/export_issue_work_hours`,
      params: {
        organizationId: this.orgId,
      },
      data,
    });
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
        content,
        self: false,
      },
    });
  }
}

const workingHoursApi = new WorkingHoursApi();
const workingHoursApiConfig = new WorkingHoursApi(true);
export { workingHoursApi, workingHoursApiConfig };
