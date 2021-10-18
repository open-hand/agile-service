import { axios } from '@choerodon/boot';
import queryString from 'query-string';
import { getProjectId } from '@/utils/common';

type IGanttDimension = 'task' | 'assignee' | 'sprint' | 'epic';
export type IGanttMoveRequestDataPreviousWithNext = {
  previousId?: string
  nextId: string
} | {
  previousId: string
  nextId?: string
}
export type IGanttMoveRequestData = {
  dimension: IGanttDimension
  currentId: string
  instanceId?: string
  instanceType?: string
  // searchVO:any
} & IGanttMoveRequestDataPreviousWithNext
class GanttApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  async loadByTask(data: any, sort?: any) {
    let result: any[] = [];
    let hasNextPage = true;
    let page = 0;
    while (hasNextPage) {
      // eslint-disable-next-line no-await-in-loop
      const res = (await this.loadByTaskPage(data, page += 1, sort));
      hasNextPage = res.hasNextPage;
      result = [...result, ...res.list];
    }
    return result;
  }

  loadByTaskPage(data: any, page: number, sort?: any) {
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
      },
    });
  }

  loadHeaders() {
    return axios({
      method: 'get',
      url: `${this.prefix}/headers/gantt_chart`,
    });
  }

  loadInfluenceIssues(dimension: string, issueIds: string[], displayFields: { code: string, projectId?: string }[]) {
    return axios({
      method: 'post',
      url: `${this.prefix}/gantt/list_by_ids`,
      params: { dimension },
      data: { issueIds, displayFields },
    });
  }

  loadDimensionRank(searchVO: any) {
    return axios({
      method: 'post',
      url: `${this.prefix}/gantt/list_dimension`,
      data: searchVO,
    });
  }

  move(dragData: IGanttMoveRequestData, searchVO: any) {
    return axios({
      method: 'post',
      url: `${this.prefix}/gantt/move`,
      data: { ...dragData, searchVO },
    });
  }

  moveTopDimension(dragData: Omit<IGanttMoveRequestData, 'instanceId' | 'instanceType'>, searchVO: any) {
    return axios({
      method: 'post',
      url: `${this.prefix}/gantt/move_dimension`,
      data: { ...dragData, searchVO },
    });
  }
}

const ganttApi = new GanttApi();

export { ganttApi };
