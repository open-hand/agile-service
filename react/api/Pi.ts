import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

class PiApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }
  /**
   * 根据状态获取PI
   * @param statusList 
   */
  getByStatus(statusList = ['todo', 'doing', 'done']) {
    return axios.post(`${this.prefix}/pi/query_pi_by_status`, statusList);
  }
  /**
   * 获取没有结束的PI
   */
  getUnfinished() {
    return axios.get(`${this.prefix}/pi/unfinished`);
  }
  /**
   * 在子项目获取当前PI
   * @param programId 
   * @param artId 
   */
  getCurrent(programId: number, artId: number) {
    return axios.get(`${this.prefix}/pi/query_doing_pi?program_id=${programId}&art_id=${artId}`);
  }
}

const piApi = new PiApi();
export { piApi };
