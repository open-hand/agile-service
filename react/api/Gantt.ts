import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

class GanttApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  loadByTask() {
    return axios({
      method: 'post',
      url: `${this.prefix}/gantt/list/by_task`,
      data: {},
    });
  }

  loadByUser() {
    return axios({
      method: 'post',
      url: `${this.prefix}/gantt/list/by_user`,
      data: {},
    });
  }

  loadHeaders() {
    return axios({
      method: 'get',
      url: `${this.prefix}/headers/gantt_chart`,
    });
  }
}

const ganttApi = new GanttApi();

export { ganttApi };
