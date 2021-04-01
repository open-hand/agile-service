import { IStatus } from '@/common/types';
import Api from './Api';

export interface IKanbanTemplateCreate {
  name: string
}
export interface IKanbanTemplateEdit {
  name: string
  boardId: string
  objectVersionNumber: number
}
export interface IKanbanTemplateStatus {
  statusId: string;
  name: string;
  categoryCode: IStatus['valueCode'];
  position: number;
  templateCompleted: boolean;
  organizationId?: string;
}

export interface IKanbanTemplateColumnCreate {
  projectId: 0
  boardId: string
  name: string
  categoryCode: IStatus['valueCode']
}
export interface IKanbanTemplateColumn {
  columnId: string;
  name: string;
  boardId: string;
  // minNum?: number;
  // maxNum?: any;
  categoryCode: IStatus['valueCode'];
  projectId?: any;
  sequence: number;
  color?: any;
  colorCode?: any;
  statusId?: any;
  objectVersionNumber: number;
  status: IKanbanTemplateStatus[];
}
export interface IKanbanTemplateMoveStatus {
  columnId: string,
  originColumnId: string,
  position: number
}
export interface IKanbanTemplateMoveColumn {
  projectId: 0,
  boardId: string,
  columnId: string,
  sequence: number,
  objectVersionNumber: number
}
export interface IKanbanTemplateColumnUpdate {
  projectId: 0,
  boardId: string,
  columnId: string,
  name: string,
  objectVersionNumber: number
}
class KanbanTemplateApi extends Api<KanbanTemplateApi> {
  get prefix() {
    return `/agile/v1/organizations/${this.orgId}`;
  }

  list(page: number, size: number) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/board_template/board/list`,
      params: {
        page,
        size,
      },
    });
  }

  checkEnableCreate() {
    return this.request({
      method: 'get',
      url: `${this.prefix}/organization_config/check_config_template`,
    });
  }

  create(data: IKanbanTemplateCreate) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/board_template/board/create`,
      params: {
        boardName: data.name,
      },
    });
  }

  checkName(name: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/board_template/board/check_name`,
      params: {
        boardName: name,
      },
    });
  }

  edit(boardId: string, data: IKanbanTemplateEdit) {
    return this.request({
      method: 'put',
      url: `${this.prefix}/board_template/board/${boardId}`,
      data,
    });
  }

  delete(boardId: string) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/board_template/board/${boardId}`,
    });
  }

  detail(boardId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/board_template/board/${boardId}`,
    });
  }

  createColumn(categoryCode: IStatus['code'], data: IKanbanTemplateColumnCreate) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/board_template/board_column/create`,
      params: {
        categoryCode,
      },
      data,
    });
  }

  columns(boardId: string): Promise<IKanbanTemplateColumn[]> {
    return this.request({
      method: 'get',
      url: `${this.prefix}/board_template/board_column/${boardId}/list`,
    });
  }

  unsetColumn(boardId: string): Promise<IStatus[]> {
    return this.request({
      method: 'get',
      url: `${this.prefix}/board_template/status_template/list_uncorrespond_status`,
      params: {
        boardTemplateId: boardId,
      },
    });
  }

  moveStatus(statusId: string, data: IKanbanTemplateMoveStatus) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/board_template/board_column/${statusId}/move_to_column`,
      data,
    });
  }

  moveStatusToUnset(statusId: string, data: { columnId: string }) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/board_template/board_column/${statusId}/move_to_uncorrespond`,
      data,
    });
  }

  moveColumn(data: IKanbanTemplateMoveColumn) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/board_template/board_column/column_sort`,
      data,
    });
  }

  updateColumn(boardId: string, columnId: string, data: IKanbanTemplateColumnUpdate) {
    return this.request({
      method: 'put',
      url: `${this.prefix}/board_template/board_column/${columnId}`,
      data,
      params: {
        boardId,
      },
    });
  }

  deleteColumn(columnId: string) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/board_template/board_column/${columnId}`,
    });
  }

  updateStatusComplete(statusId: string, completed: boolean) {
    return this.request({
      method: 'put',
      url: `${this.prefix}/board_template/board_column/${statusId}/status_template_completed`,
      params: {
        completed,
      },
    });
  }
}

const kanbanTemplateApi = new KanbanTemplateApi();
const kanbanTemplateApiConfig = new KanbanTemplateApi(true);
export { kanbanTemplateApi, kanbanTemplateApiConfig };
