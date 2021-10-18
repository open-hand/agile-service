import { getOrganizationId, getProjectId } from '@/utils/common';
import Api from './Api';

interface ILogData {
  userIds: string[],
  projectIds?: string[],
  startTime: string,
  endTime: string,
}

class WorkingHoursApi extends Api<WorkingHoursApi> {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  get OrgPrefix() {
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
      url: `${this.OrgPrefix}/work_hours/work_hours_log`,
      params: {
        ...(params || {}),
      },
      data,
    });
  }
}

const workingHoursApi = new WorkingHoursApi();
const workingHoursApiConfig = new WorkingHoursApi(true);
export { workingHoursApi, workingHoursApiConfig };
