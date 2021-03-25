import Api from './Api';

export interface IKanbanTemplateCreate {
  name: string
  description?: string
}
export interface IKanbanTemplateEdit {
  name: string
  description?: string
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

  create(data: IKanbanTemplateCreate) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/board_template/board/create`,
    });
  }

  edit(boardId: string, data: IKanbanTemplateEdit) {
    return this.request({
      method: 'put',
      url: `${this.prefix}//board_template/board/${boardId}`,
      data,
    });
  }
}

const kanbanTemplateApi = new KanbanTemplateApi();
const kanbanTemplateApiConfig = new KanbanTemplateApi(true);
export { kanbanTemplateApi, kanbanTemplateApiConfig };
