import { axios } from '@choerodon/boot';
import { getProjectId } from '../common/utils';

interface ISprint {
  sprintName: string
  startDate?: string
  endDate?: string
  sprintGoal?: string
}

class SprintApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }
  /**
   * 创建冲刺
   * @param sprint 
   */
  create(sprint: ISprint) {
    return axios.post(`${this.prefix}/sprint`, sprint);
  }
  /**
   * 在当前PI下创建冲刺
   * @param sprint 
   */
  createOnCurrentPi(sprint: ISprint) {
    return axios.post(`${this.prefix}/sprint/sub_project`, sprint);
  }
  /**
   * 校验冲刺名称
   * @param name 
   */
  validate(name: string) {
    return axios.post(`${this.prefix}/sprint/check_name`, {
      sprintName: name,
    });
  }
  /**
   * 根据piId查询冲刺
   * @param piId 
   */
  getAllByPiId(piId: number) {
    return axios.get(`${this.prefix}/sprint/sub_project/list?pi_id=${piId}`);
  }
}

const sprintApi = new SprintApi()
export { sprintApi };
