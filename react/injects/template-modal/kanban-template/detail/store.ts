/* eslint-disable no-param-reassign */
import {
  observable, action, runInAction, computed,
} from 'mobx';
import { kanbanTemplateApi, IKanbanTemplateColumn } from '@/api';

export default class KanbanTemplateDetailStore {
  constructor(templateId: string) {
    this.templateId = templateId;
  }

  templateId: string

  @observable loading = false

  @observable columns: IKanbanTemplateColumn[] = []

  @computed get allColumns():IKanbanTemplateColumn[] {
    return this.columns;
  }

  @action
  async refresh() {
    this.loading = true;
    if (this.templateId) {
      const res = await kanbanTemplateApi.columns(this.templateId);
      runInAction(() => {
        this.columns = res;
        this.loading = false;
      });
    }
  }
}
