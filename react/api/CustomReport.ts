import { IChartType, IChartUnit } from '@/routes/ReportHost/custom-report/components/Chart/utils';
import { getOrganizationId } from '@/utils/common';
import { axios } from '@choerodon/boot';
import Api from './Api';

export interface ICreateData {
  searchJson?: string
  name: string
  description?: string
  chartType: IChartType
  statisticsType: IChartUnit
  analysisField: string
  analysisFieldPredefined: boolean,
  comparedField?: string,
  comparedFieldPredefined?: boolean,
  objectVersionNumber?:number
}
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

  createChart(data: ICreateData) {
    return axios({
      method: 'post',
      url: `${this.prefix}/custom_chart`,
      data,
    });
  }

  updateChart(chartId: string, data: ICreateData) {
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

  deleteChart(chartId: string) {
    return axios({
      method: 'delete',
      url: `${this.prefix}/custom_chart/${chartId}`,
    });
  }
}

const customReportApi = new CustomReportApi();
export { customReportApi };
