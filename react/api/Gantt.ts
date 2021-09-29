import { axios } from '@choerodon/boot';
import queryString from 'query-string';
import { getProjectId } from '@/utils/common';

type IGanttDimension = 'task' | 'assignee' | 'sprint' | 'epic';
class GanttApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  async loadByTask(data: any, dimension: IGanttDimension, sort?: any) {
    let result: any = [];
    let hasNextPage = true;
    let page = 0;
    while (hasNextPage) {
      // eslint-disable-next-line no-await-in-loop
      const res = await this.loadByTaskPage(data, dimension, page += 1, sort);
      hasNextPage = res.hasNextPage;
      result = [...result, ...res.list];
    }
    return result;
  }

  loadByTaskPage(data: any, dimension: IGanttDimension, page: number, sort?: any) {
    return axios({
      method: 'post',
      url: `${this.prefix}/gantt/list`,
      data,
      paramsSerializer(params: any) {
        return queryString.stringify(params);
      },
      params: {
        size: 1000,
        page,
        sort,
        dimension,
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
