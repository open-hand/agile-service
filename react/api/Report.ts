import { axios } from '@choerodon/boot';
import { getOrganizationId } from '@/utils/common';
import { ISearchVO } from '@/common/types';
import Api from './Api';

interface ICumulativeData {
  columnIds: Array<string>, // 看板列id
  endDate: string,
  quickFilterIds: Array<string> | [], // 快速搜索的id列表
  startDate: string,
  boardId: string,
}
class ReportApi extends Api<ReportApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  /**
   * 根据冲刺id查询冲刺燃尽图详情
   * @param sprintId
   * @param type  storyPoints、remainingEstimatedTime、issueCount
   * @param ordinalType  asc,desc
   */
  loadSprintBurnDown(sprintId: number, type: string, ordinalType = 'asc', filter: {
    assigneeId?: string
    onlyStory?: boolean
    quickFilterIds?: string[]
  } = {}, searchVO: ISearchVO) {
    const { assigneeId, onlyStory, quickFilterIds } = filter;
    return axios({
      method: 'post',
      url: `${this.prefix}/reports/${sprintId}/burn_down_report`,
      data: {
        type,
        ordinalType,
        assigneeId,
        onlyStory: onlyStory ? true : undefined,
        quickFilterIds,
        currentSearchVO: searchVO,
      },
    });
  }

  /**
   * 查询燃尽图坐标信息
   * @param sprintId
   * @param type
   */
  loadBurnDownCoordinate(sprintId: string, type: string, filter: {
    assigneeId?: string
    onlyStory?: boolean
    quickFilterIds?: string[]
    personalFilterIds?: string[]
  } = {}, searchVO?: ISearchVO) {
    const {
      assigneeId, onlyStory, quickFilterIds, personalFilterIds,
    } = filter;
    return this.request({
      method: 'post',
      url: `${this.prefix}/reports/${sprintId}/burn_down_report/coordinate`,
      data: {
        type,
        assigneeId,
        onlyStory: onlyStory ? true : undefined,
        quickFilterIds,
        personalFilterIds,
        currentSearchVO: searchVO,
      },
    });
  }

  /**
   *
   * 加载累积流量图信息
   * @param data
   */
  loadCumulativeData(data: ICumulativeData) {
    return axios.post(`${this.prefix}/reports/cumulative_flow_diagram`, data);
  }

  /**
   * 加载版本报告图
   * @param versionId
   * @param type
   */
  loadVersionChart(versionId: string, type: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/reports/version_chart`,
      params: {
        versionId,
        type,
      },
    });
  }

  /**
   * 加载版本报告工作项列表
   * @param versionId
   */
  loadVersionTable(versionId: string) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'get',
      url: `${this.prefix}/reports/version_issue_list`,
      params: {
        organizationId,
        versionId,
      },
    });
  }

  /**
   * 加载史诗或版本燃耗图信息
   * @param id
   * @param type Epic Version
   */
  loadEpicOrVersionBurnDown(id: number, type: string) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'get',
      url: `${this.prefix}/reports/burn_down_report_type/${id}`,
      params: {
        organizationId,
        type,
      },
    });
  }

  /**
   * 加载史诗或版本燃耗图坐标信息
   * @param id
   * @param type  Epic Version
   */
  loadEpicOrVersionBurnDownCoordinate(id: string, type: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/reports/burn_down_coordinate_type/${id}`,
      params: {
        type,
      },
    });
  }

  /**
   * 加载史诗图
   * @param epicId
   * @param type
   */
  loadEpicChart(epicId: string, type: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/reports/epic_chart`,
      params: {
        epicId,
        type,
      },
    });
  }

  /**
   * 加载史诗图工作项列表
   * @param epicId
   */
  loadIssuesForEpic(epicId: string) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'get',
      url: `${this.prefix}/reports/epic_issue_list`,
      params: {
        epicId,
        organizationId,
      },
    });
  }

  /**
   * 加载速度图
   * @param type
   */
  loadVelocity(type: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/reports/velocity_chart`,
      params: {
        type,
      },
    });
  }

  /**
   * 加载饼图
   * @param versionId
   * @param type
   */
  loadPie(fieldName: string, sprintId?: string, versionId?: string, statusId?: string, startDate?: string, endDate?: string) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'get',
      url: `${this.prefix}/reports/pie_chart`,
      params: {
        organizationId,
        fieldName,
        sprintId,
        versionId,
        statusId,
        startDate,
        endDate,
      },
    });
  }
}

const reportApi = new ReportApi();
export { reportApi };
