import { axios } from '@choerodon/boot';
import Api from './Api';

class RdqamApi extends Api<RdqamApi> {
  get prefix() {
    return '/rdqam/v1';
  }

  load() {
    return axios({
      method: 'get',
      url: `${this.prefix}/${this.orgId}/${this.projectId}/dashboard/sonar_issue`,
      params: {
        page: 1,
        size: 10,
      },
    });
  }

  loadHistory(startDate: string, endDate: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/${this.orgId}/${this.projectId}/dashboard/sonar_issue_history`,
      params: {
        startDate,
        endDate,
      },
    });
  }

  loadByService({
    serviceId, startDate, endDate, type,
  }: { serviceId: string, startDate: string, endDate: string, type: 'issue' | 'coverage' | 'duplicate' }) {
    return axios({
      method: 'get',
      url: `${this.prefix}/${this.orgId}/${this.projectId}/dashboard/${serviceId}/sonarqube_table`,
      params: {
        startTime: startDate,
        endTime: endDate,
        type,
      },
    });
  }
}

const rdqamApi = new RdqamApi();
export { rdqamApi };
