import { getProjectId, getOrganizationId } from '@/utils/common';
import Api from './Api';

class ProjectReportApi extends Api {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  load() {
    return this.request({
      method: 'get',
      url: `${this.prefix}/project_report`,
    });
  }
}

const projectReportApi = new ProjectReportApi();
const projectReportApiConfig = new ProjectReportApi(true);
export { projectReportApi, projectReportApiConfig };
