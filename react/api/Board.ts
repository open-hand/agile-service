import { omit } from 'lodash';
import { axios, stores, Choerodon } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '@/utils/common';
import Api from './Api';

const { AppState } = stores;

interface ICardStatus {
  completed: boolean,
  id: number,
  objectVersionNumber: number,
  projectId: number,
  statusId: number,
}
interface UMoveStatus {
  columnId: number,
  originColumnId: number,
  position: number,
  statusObjectVersionNumber: number,
}
interface UBoard {
  boardId: number,
  columnConstraint: string, // 列约束
  objectVersionNumber: number,
  projectId: number,
}
interface BoardSearchVO {
  onlyMe?: boolean,
  onlyStory?: boolean,
  assigneeId?: number,
  quickFilterIds?: Array<number>,
  assigneeFilterIds?: Array<number>,
  sprintId?: number,
  personalFilterIds?: string[]
  priorityIds?: string[]
}
export interface IStatusLinkage {
  issueTypeId: string
  statusId: string
}
/**
 * 迭代看板
 * @author dzc
 */
class BoardApi extends Api<BoardApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  load(boardId: string, searchVO: any) {
    return axios({
      method: 'post',
      url: `${this.prefix}/board/${boardId}/all_data/${getOrganizationId()}`,
      data: searchVO || {},
    });
  }

  /**
       * 加载看板列表
       */
  loadAll() {
    return axios.get(`${this.prefix}/board`);
  }

  getStatusLinkages(): Promise<IStatusLinkage[]> {
    return axios.get(`${this.prefix}/status_linkages/list_by_project`);
  }

  /**
   * 加载看板中未对应的状态
   * @param boardId
   */
  loadNoColumnStatus(boardId: number, applyType: string = 'agile') {
    return axios({
      method: 'get',
      url: `${this.prefix}/issue_status/list_by_options`,
      params: {
        boardId,
        applyType,
      },
    });
  }

  /**
   * 创建一个看板
   * @param boardName
   */
  create(boardName: string) {
    return axios({
      method: 'post',
      url: `${this.prefix}/board`,
      params: {
        boardName,
      },
    });
  }

  /**
    * 更新看板
    * @param boardId
    * @param data
    */
  update(boardId: number, data: UBoard) {
    return axios({
      method: 'put',
      url: `${this.prefix}/board/${boardId}`,
      data,
    });
  }

  /**
    * 配置看板中状态卡片的更新 （目前用于设置是否完成）
    * @param id
    * @param data
    */
  updateStatus(id: number, data: ICardStatus) {
    return this.request({
      method: 'put',
      url: `${this.prefix}/issue_status/${id}`,
      data,
    });
  }

  /**
      * 更新用户泳道设置
      * @param boardId
      * @param swimlaneBasedCode
      */
  updateUserSetting(boardId: number, swimlaneBasedCode: string) {
    return axios({
      method: 'post',
      url: `${this.prefix}/board/user_setting/${boardId}`,
      params: {
        swimlaneBasedCode,
      },
    });
  }

  /**
         * 删除迭代看板
         * @param boardId
         */
  delete(boardId: number) {
    return axios.delete(`${this.prefix}/board/${boardId}`);
  }

  /** 删除迭代看板中未对应的状态
   *
   * @param statusId
   * @param applyType
   */
  deleteStatus(statusId: number, applyType: string = 'agile') {
    return axios({
      method: 'delete',
      url: `${this.prefix}/issue_status/${statusId}`,
      params: {
        applyType,
      },
    });
  }

  /**
   * 移动迭代看板issue
   * @param issueId
   * @param transformId
   * @param data
   */
  moveIssue(issueId: number, transformId: number, data: any) {
    return axios({
      method: 'post',
      url: `${this.prefix}/board/issue/${issueId}/move`,
      params: {
        transformId,
      },
      data,
      noPrompt: true,
    }).catch((err:any) => {
      const { code } = err || {};
      if (code === 'error.stateMachine.executeTransform') {
        Choerodon.prompt('该工作项项状态已被修改，请刷新看板', 'error');
      } else {
        Choerodon.prompt(err.message, 'error');
      }
      return err;
    });
  }

  /**
   * 移动状态到未对应列
   * @param statusId
   * @param data
   */
  moveStatusToUnset(statusId: number, data: UMoveStatus) {
    return axios.post(`${this.prefix}/issue_status/${statusId}/move_to_uncorrespond`, data);
  }

  /**
   * 移动状态到列中
   * @param code
   * @param data
   */
  moveStatusToColumn(statusId: number, data: UMoveStatus) {
    return axios.post(`${this.prefix}/issue_status/${statusId}/move_to_column`, data);
  }

  /**
    * 检查看板名称是否重复
    * @param boardName
    */
  checkName(boardName: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/board/check_name`,
      params: {
        boardName,
      },
    });
  }

  /**
   * 根据issueId获取不可拖进的看板状态
   * 用于受子级限制的父级任务
   * @param issueId
   */
  getNotAllowedTransferStatus(issueId: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/status_transfer_setting/not_allowed_transfer`,
      params: {
        issueId,
      },
    });
  }
}

const boardApi = new BoardApi();
const boardApiConfig = new BoardApi(true);
export { boardApi, boardApiConfig };
