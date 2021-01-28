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
}

const rdqamApi = new RdqamApi();
export { rdqamApi };
