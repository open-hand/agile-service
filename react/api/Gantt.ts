import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

class GanttApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  async loadByTask(data: any) {
    let result: any = [];
    let hasNextPage = true;
    let page = 0;
    while (hasNextPage) {
      // eslint-disable-next-line no-await-in-loop
      const res = await this.loadByTaskPage(data, page += 1);
      hasNextPage = res.hasNextPage;
      result = [...result, ...res.list];
    }
    return result;
  }

  loadByTaskPage(data: any, page: number) {
    return axios({
      method: 'post',
      url: `${this.prefix}/gantt/list`,
      data,
      params: {
        size: 1000,
        page,
      },
    });
  }

  loadHeaders() {
    return axios({
      method: 'get',
      url: `${this.prefix}/headers/gantt_chart`,
    });
  }

  loadInfluenceIssues(issueIds: string[]) {
    return axios({
      method: 'post',
      url: `${this.prefix}/gantt/list_by_ids`,
      data: issueIds,
    });
  }
}

const ganttApi = new GanttApi();

export { ganttApi };
