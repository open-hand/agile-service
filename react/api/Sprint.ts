import { axios } from '@choerodon/boot';
import { getOrganizationId } from '@/utils/common';
import { ISearchVO } from '@/common/types';
import Api from './Api';

interface ISprint {
  sprintName: string
  startDate?: string
  endDate?: string
  sprintGoal?: string
}
interface USprint {
  sprintId: number,
  objectVersionNumber: number,
  projectId: number,
  sprintName: string,
  sprintGoal?: string,
  startDate?: string
  endDate?: string
}
interface CloseSprint {
  incompleteIssuesDestination: number,
  projectId: number,
  sprintId: number,
}
interface StartSprint {
  endDate: string,
  startDate: string,
  projectId: number,
  sprintGoal: string,
  sprintId: number,
  sprintName: string,
  objectVersionNumber: number,
  workDates: Array<any>,
}
interface advancedSearch {
  advancedSearchArgs: object,
}
interface MoveIssueCardsInfo {
  before: boolean, // 是否移动到前面
  issueIds: Array<number>, // 待移动的工作项ids
  outsetIssueId: number, // 移动参照工作项id 0代表无工作项
  rankIndex: number, // 是否生成移动日志
}
export interface SprintStartCheckInfo {
  noRemainingTimeIssue: number
  noStoryPointIssue: number
}
class SprintApi extends Api<SprintApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
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

  /**
 * 根据冲刺状态数组获取冲刺，["started", "sprint_planning", "closed"]，不论是普通项目还是子项目都可以
 * @param {*} arr
 */
  loadSprints(arr: Array<string> = [], projectId?: number) {
    return this.request({
      method: 'post',
      url: `/agile/v1/projects/${projectId || this.projectId}/sprint/names`,
      data: arr,
    });
  }

  /**
   * 加载全部子项目下的冲刺
   * @param param
   * @param page
   * @param selectedIds
   */
  loadSubProjectSprints(param: string, page: number, selectedIds?: number[], size = 10) {
    return axios({
      method: 'post',
      url: `${this.prefix}/sprint/sub_project/page_sprints`,
      params: {
        page,
        size,
        param,
      },
      data: selectedIds,
    });
  }

  /**
   * 根据冲刺id加载冲刺
   * @param sprintId
   */
  loadSprint(sprintId: number) {
    return axios.get(`${this.prefix}/sprint/${sprintId}`);
  }

  /**
   * 根据冲刺id查询sprint名及此冲刺下issue统计信息
   * @param sprintId
   */
  loadSprintAndCountIssue(sprintId: number) {
    return axios.get(`${this.prefix}/sprint/${sprintId}/names`);
  }

  /**
   * 查询未关闭的冲刺
   */
  loadUncloseSprint() {
    return axios.get(`${this.prefix}/sprint/unclosed`);
  }

  /**
   * 根据冲刺id及状态加载工作项
   * @param sprintId
   * @param status
   * @param page
   * @param size
   */
  loadSprintIssues(sprintId: number, status: string, page: number = 1, size: number = 99999) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'get',
      url: `${this.prefix}/sprint/${sprintId}/issues`,
      params: {
        organizationId,
        status,
        page,
        size,
      },
    });
  }

  /**
   * 根据团队id及PI id 加载冲刺列表
   * @param teamId
   * @param piId
   */
  loadSprintsByTeam(teamId: number, piId: number) {
    return axios(
      {
        method: 'get',
        url: `${this.prefix}/sprint/sub_project/${teamId}/list_by_team_id`,
        params: {
          piId,
        },
      },
    );
  }

  /**
   * 按团队Ids和piId查询冲刺
   * @param {*} piId
   * @param {*} teamIds
   */
  getTeamSprints(piId: number, teamIds: Array<number>) {
    return axios({
      method: 'get',
      url: `${this.prefix}/sprint/sub_project/list_by_team_ids`,
      params: {
        piId,
        teamIds: teamIds.join(','),
      },
    });
  }

  /**
   * 更新冲刺部分字段
   * @param data
   * @param isCurrentPi  更新的冲刺是否为项目群下的子项目
   */
  updateSprint(data: USprint, isCurrentPi: boolean = false) {
    return axios({
      method: 'put',
      url: `${this.prefix}/sprint${isCurrentPi ? '/sub_project' : ''}`,
      data,
    });
  }

  /**
   * 完成冲刺
   * @param data
   */
  complete(data: CloseSprint) {
    return axios.post(`${this.prefix}/sprint/complete`, data);
  }

  /**
   * 开启冲刺
   * @param data
   * @param isCurrentPi 开启的冲刺是否为项目群下的子项目
   */
  checkSprintBeforeStart(sprintId: string): Promise<SprintStartCheckInfo> {
    return axios({
      method: 'get',
      url: `${this.prefix}/sprint/sprint_start_message`,
      params: {
        sprintId,
      },
    });
  }

  /**
   * 开启冲刺
   * @param data
   * @param isCurrentPi 开启的冲刺是否为项目群下的子项目
   */
  start(data: StartSprint, isCurrentPi = false) {
    return axios.post(`${this.prefix}/sprint/${isCurrentPi ? 'sub_project/' : ''}start`, data);
  }

  /**
   * 删除冲刺
   * @param sprintId
   * @param isCurrentPi 删除的冲刺是否为项目群下的子项目
   */
  delete(sprintId: number, isCurrentPi = false) {
    return axios.delete(`${this.prefix}/sprint/${isCurrentPi ? 'sub_project/' : ''}${sprintId}`);
  }

  /**
   * 联合查询sprint及其issue
   * @param quickFilterIds 快速查询ids
   * @param assigneeFilterIds 经办人搜索ids
   * @param filter  过滤条件
   */
  getSprintAndIssues(quickFilterIds: Array<number>, assigneeFilterIds: Array<number>, filter: advancedSearch) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'post',
      url: `${this.prefix}/sprint/issues`,
      data: filter,
      params: {
        organizationId,
        quickFilterIds,
        [assigneeFilterIds.length > 0 ? 'assigneeFilterIds' : '']: assigneeFilterIds,
      },
    });
  }

  /**
   * 根据冲刺id查询冲刺的时间范围内非工作日(包含周六周天)
   * @param sprintId
   */
  getRestDays(sprintId: string): Promise<string[]> {
    return axios.get(`${this.prefix}/sprint/query_non_workdays/${sprintId}/${getOrganizationId()}`);
  }

  /**
   * 根据冲刺id查询经办人分布状况
   * @param sprintId
   */
  getAssigneeDistribute(sprintId: number) {
    return axios({
      method: 'get',
      url: `${this.prefix}/iterative_worktable/assignee_id`,
      params: {
        sprintId,
      },
    });
  }

  /**
   * 根据冲刺id查询工作项类型分布状况
   * @param sprintId
   */
  getIssueTypeDistribute(sprintId: number) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'get',
      url: `${this.prefix}/iterative_worktable/issue_type`,
      params: {
        sprintId,
        organizationId,
      },
    });
  }

  /**
   * 冲刺id联合组织id查询冲刺基本信息
   * @param sprintId
   */
  getSprintCombineOrgId(sprintId: number) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'get',
      url: `${this.prefix}/iterative_worktable/sprint/${organizationId}`,
      params: {
        sprintId,
      },
    });
  }

  /**
   * 根据冲刺id查询状态分布状况
   * @param sprintId
   */
  getStatusDistribute(sprintId: number) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'get',
      url: `${this.prefix}/iterative_worktable/status`,
      params: {
        organizationId,
        sprintId,
      },
    });
  }

  /**
   * 将批量的issue加入到冲刺中
   * @param sprintId
   * @param issueIds
   */
  addIssues(sprintId: number, moveCardsInfo: MoveIssueCardsInfo) {
    return axios.post(`${this.prefix}/issues/to_sprint/${sprintId}`, moveCardsInfo);
  }

  loadSprintsWidthInfo(sprintIds: string[]) {
    return axios({
      method: 'post',
      url: `${this.prefix}/story_map/sprint_info`,
      params: {
        organizationId: getOrganizationId(),
      },
      data: sprintIds,
    });
  }

  /**
   * 改变（删除、完成）冲刺前检查默认值是否为｛sprintId｝冲刺
   * @param sprintId
   */
  beforeChangeCheck(sprintId: string): Promise<boolean> {
    return axios({
      method: 'get',
      url: `${this.prefix}/sprint/check_default_value`,
      params: {
        sprintId,
      },

    });
  }

  getBacklogSprintsInfo(searchVO: ISearchVO): Promise<any> {
    return this.request({
      method: 'post',
      url: `${this.prefix}/sprint/unclose_sprint`,
      params: {
        organizationId: this.orgId,
      },
      data: searchVO,
    });
  }

  getIssuesBySprintId(sprintId: string, searchVO: ISearchVO, page: number, size: number): Promise<any> {
    return this.request({
      method: 'post',
      url: `${this.prefix}/sprint/${String(sprintId) === '0' ? 'todo_issue_page' : 'sprint_issue_page'}`,
      params: {
        sprintId,
        organizationId: this.orgId,
        page,
        size,
      },
      data: searchVO,
    });
  }
}

const sprintApi = new SprintApi();
const sprintApiConfig = new SprintApi(true);

export { sprintApi, sprintApiConfig };
