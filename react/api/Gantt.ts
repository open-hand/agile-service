import { axios } from '@choerodon/boot';
import queryString from 'query-string';
import { getProjectId, getOrganizationId } from '@/utils/common';
import Api from './Api';

type IGanttDimension = 'task' | 'assignee' | 'sprint' | 'epic';
export type IGanttMoveRequestDataPreviousWithNext = {
  previousId?: string
  nextId: string
} | {
  previousId: string
  nextId?: string
}
export interface IGanttConflictAssignee {
  userId: string
  conflicted: boolean
}
export interface IGanttSortDataItem {
  property: string,
  direction: 'desc' | 'asc'
}
export type IGanttMoveRequestData = {
  dimension: IGanttDimension
  currentId: string
  instanceId?: string
  instanceType?: string
  // searchVO:any
} & IGanttMoveRequestDataPreviousWithNext
class GanttApi extends Api<GanttApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  get orgPrefix() {
    return `/agile/v1/organizations/${this.orgId}`;
  }

  async loadOrgByTask(data: any, page: number, sort?: any) {
    const res = await axios({
      method: 'post',
      url: `${this.orgPrefix}/gantt/list`,
      data,
      paramsSerializer(params: any) {
        return queryString.stringify(params);
      },
      params: {
        size: 10,
        page,
        sort,
      },
    });
    return res.list;
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

  /**
   * 查询组织下所有敏捷项目
   * @param filter
   * @returns
   */
  loadProjects(filter?: any) {
    return this.request({
      method: 'get',
      url: `${this.orgPrefix}/gantt/agile_projects`,
      params: {
        name: '',
        code: '',
        param: filter || '',
      },
    });
  }

  loadByTaskPage(data: any, page: number, sort?: any) {
    return this.request({
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
    return this.request({
      method: 'get',
      url: `${this.prefix}/headers/gantt_chart`,
    });
  }

  loadInfluenceIssues(dimension: string, issueIds: string[], displayFields: { code: string, projectId?: string }[]) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/gantt/list_by_ids`,
      params: { dimension },
      data: { issueIds, displayFields },
    });
  }

  loadDimensionRank(searchVO: any) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/gantt/list_dimension`,
      data: searchVO,
    });
  }

  move(dragData: IGanttMoveRequestData, searchVO: any) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/gantt/move`,
      data: { ...dragData, searchVO },
    });
  }

  moveTopDimension(dragData: Omit<IGanttMoveRequestData, 'instanceId' | 'instanceType'>, searchVO: any) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/gantt/move_dimension`,
      data: { ...dragData, searchVO },
    });
  }

  /**
   *  组织层甘特图 经办人视图查询时间冲突的经办人
   * @param searchVO
   * @returns
   */
  loadTimeConflict(searchVO: any): Promise<IGanttConflictAssignee[]> {
    return this.request({
      method: 'post',
      url: `${this.orgPrefix}/gantt/estimated_time/conflict`,
      data: searchVO,
    });
  }

  /**
   * 组织层甘特图查询冲突详情
   * @param assigneeId
   * @param data
   * @param page
   * @param size
   * @returns
   */
  loadTimeConflictDetail(assigneeId: string, data: any = {}, page: number = 1, size: number = 10) {
    return this.request({
      method: 'post',
      url: `${this.orgPrefix}/gantt/estimated_time/conflict/details`,
      params: { assigneeId, page, size },
      data: {
        contents: [data.contents].filter(Boolean),
        searchArgs: {
          teamProjectIds: data.teamProjectIds,
        },
      },
    });
  }

  saveSort(sortData: IGanttSortDataItem[]) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/gantt/save/sort`,
      data: sortData,
    });
  }

  loadSort() {
    return this.request({
      method: 'get',
      url: `${this.prefix}/gantt/latest_sort`,
    });
  }
}

const ganttApi = new GanttApi();
const ganttApiConfig = new GanttApi(true);
export { ganttApi, ganttApiConfig };
