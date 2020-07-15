import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

interface UWsjf {
    id: number,
    issueId: number,
    objectVersionNumber: number,
    jobSize: number, // 工作规模
    rrOeValue: number, // 降低风险|促成机会
    timeCriticality: number, // 时间紧迫性
    userBusinessValue: number, // 用户/业务价值
    costDelay?: number, // 延迟成本
}
class WsjfApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  /**
     * 更新wsjf
     * @param data 
     */
  update(data: UWsjf) {
    return axios.post(`${this.prefix}/wsjf`, data);
  }
}

const wsjfApi = new WsjfApi();
export { wsjfApi };
