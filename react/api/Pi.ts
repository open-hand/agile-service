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
    return axios({
      url: `${this.prefix}/pi/query_doing_pi`,
      method: 'get',
      params: {
        program_id: programId,
        art_id: artId,
      },
    });
  }

  /**
   * 子项目下,根据活跃的ART和PI状态查询PI
   * @param data  状态列表，[todo、doing、done]
   * @param programId 
   */
  getPiByPiStatus(data:Array<string>, programId:number) {
    return axios({
      method: 'post',
      url: `${this.prefix}/project_invoke_program/pi/query_pi_by_status`,
      params: {
        programId,
      },
      data,
    });
  }
}

const piApi = new PiApi();
export { piApi };
