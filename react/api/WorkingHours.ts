import { getIsOrganization, getOrganizationId, getProjectId } from '@/utils/common';
import Api from './Api';

export interface ILogData {
  userIds: string[],
  projectIds?: string[],
  startTime: string,
  endTime: string,
}

class WorkingHoursApi extends Api<WorkingHoursApi> {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  get orgPrefix() {
    return `/agile/v1/organizations/${getOrganizationId()}`;
  }

  getLogs(params: any, data: ILogData) {
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

  orgGetLogs(params: any, data: ILogData) {
    return this.request({
      method: 'post',
      url: `${this.orgPrefix}/work_hours/work_hours_log`,
      params: {
        ...(params || {}),
      },
      data,
    });
  }

  exportLog(data: ILogData) {
    return this.request({
      method: 'post',
      url: `${getIsOrganization() ? this.orgPrefix : this.prefix}/work_hours/export_work_hours_log`,
      params: {
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  getLatest() {
    return this.request({
      method: 'get',
      url: `${getIsOrganization() ? this.orgPrefix : this.prefix}/excel/latest`,
      params: {
        organizationId: getOrganizationId(),
        action: 'download_file_work_hours_log',
      },
    });
  }
}

const workingHoursApi = new WorkingHoursApi();
const workingHoursApiConfig = new WorkingHoursApi(true);
export { workingHoursApi, workingHoursApiConfig };
