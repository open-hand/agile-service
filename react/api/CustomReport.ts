import { axios } from '@choerodon/boot';
import Api from './Api';

class CustomReportApi extends Api<CustomReportApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  checkName(name: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/project_invoke_program/datalog`,
      params: {
        name,
      },
    });
  }
}

const customReportApi = new CustomReportApi();
export { customReportApi };
