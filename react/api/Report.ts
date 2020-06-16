import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';


class ReportApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  /**
   * 根据冲刺id查询冲刺燃尽图详情
   * @param sprintId 
   * @param type  storyPoints、remainingEstimatedTime、issueCount
   * @param ordinalType  asc,desc
   */
  loadSprintBurnDown(sprintId:number, type:string, ordinalType = 'asc') {
    return axios({
      method: 'get',
      url: `${this.prefix}/reports/${sprintId}/burn_down_report`,
      params: {
        type,
        ordinalType,
      },
    });
  }
}

const reportApi = new ReportApi();
export { reportApi };
