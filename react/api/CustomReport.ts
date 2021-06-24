import { getOrganizationId } from '@/utils/common';
import { axios } from '@choerodon/boot';
import Api from './Api';

class CustomReportApi extends Api<CustomReportApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  checkName(name: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/custom_chart/check_name`,
      params: {
        name,
      },
    });
  }

  getData(data: any) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/reports/custom_chart`,
      params: {
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  getCustomReports() {
    return axios({
      method: 'get',
      url: `${this.prefix}/custom_chart`,
    });
  }

  createChart(data: any) {
    return axios({
      method: 'post',
      url: `${this.prefix}/custom_chart`,
      data,
    });
  }

  updateChart(chartId: string, data: any) {
    return axios({
      method: 'put',
      url: `${this.prefix}/custom_chart/${chartId}`,
      data,
    });
  }

  getChartAllDataById(chartId: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/custom_chart/${chartId}`,
    });
  }
}

const customReportApi = new CustomReportApi();
export { customReportApi };
