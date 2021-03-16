import { getProjectId } from '@/utils/common';
import Api from './Api';

export interface IProjectReportCreate {
  ccList?: string[],
  description: string,
  projectId: string,
  receiverList: string[],
  reportUnitList: any[],
  title: string
}
export interface IProjectReportUpdate extends IProjectReportCreate {
  objectVersionNumber: number
}
class ProjectReportApi extends Api<ProjectReportApi> {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  load() {
    return this.request({
      method: 'get',
      url: `${this.prefix}/project_report`,
    });
  }

  create(data: IProjectReportCreate) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/project_report`,
      data,
    });
  }

  update(reportId: string, data: IProjectReportUpdate) {
    return this.request({
      method: 'put',
      url: `${this.prefix}/project_report/${reportId}`,
      data,
    });
  }

  getById(reportId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/project_report/${reportId}`,
    });
  }

  delete(reportId: string) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/project_report/${reportId}`,
    });
  }

  send(reportId: string, html: string) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/project_report/send/${reportId}`,
      data: {
        html,
      },
    });
  }
}

const projectReportApi = new ProjectReportApi();
const projectReportApiConfig = new ProjectReportApi(true);
export { projectReportApi, projectReportApiConfig };
