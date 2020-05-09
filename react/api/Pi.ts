import { axios } from '@choerodon/boot';
import { getProjectId } from '../common/utils';

class PiApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  getByStatus(statusList = ['todo', 'doing', 'done']) {
    return axios.post(`${this.prefix}/pi/query_pi_by_status`, statusList);
  }

  getUnfinished() {
    return axios.get(`${this.prefix}/pi/unfinished`);
  }

  getCurrent(programId: number, artId: number) {
    return axios.get(`/${this.prefix}/pi/query_doing_pi?program_id=${programId}&art_id=${artId}`);
  }
}

const piApi = new PiApi();
export { piApi };
