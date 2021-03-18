import {
  observable, action, computed, runInAction,
} from 'mobx';
import {
  ILog, Issue, IComment,
} from '@/common/types';
import { dataLogApi, fieldApi, issueApi } from '@/api';
import type { Events, DemandEvents } from './index';
import { IField } from '../custom-field';

class Store {
  @observable loading: boolean = false;

  @observable outside: boolean = false;

  @observable organizationId: string | undefined;

  @observable projectId: number | string | undefined;

  @observable selectedMap = observable.map();

  @action
  select(issueId: string, projectId?: string) {
    if (!this.selectedMap.has(issueId)) {
      this.selectedMap.clear();
      this.selectedMap.set(issueId, true);
      if (projectId) {
        this.projectId = String(projectId);
      }
    }
  }

  @action
  unSelect(issueId: string) {
    this.selectedMap.delete(issueId);
  }

  @action
  close() {
    this.selectedMap.clear();
    this.fireEvent('close', this.issue);
  }

  @action
  destroy() {
    this.selectedMap.clear();
    this.issue = {} as Issue;
    this.loading = false;
    this.outside = false;
    this.projectId = undefined;
    this.organizationId = undefined;
  }

  @computed get visible() {
    return this.selectedMap.size > 0;
  }

  @computed get selected() {
    return [...this.selectedMap.keys()][0];
  }

  @observable logs: ILog[] = [];

  @action setLogs = (data: ILog[]) => {
    this.logs = data;
  }

  @observable customFields: IField[] = [];

  @action setCustomFields = (customFields: IField[]) => {
    this.customFields = customFields;
  }

  events: Events = {}

  initEvents(events: Events) {
    this.events = events;
  }

  /**
   * api初始化， 外部与内部调用的接口在此进行判断
   * @param source
   */
  initApi(outside: boolean, organizationId?: string) {
    this.outside = outside;
    if (this.outside) {
      this.organizationId = organizationId;
    }
  }

  fireEvent(eventKey: DemandEvents, payload: any) {
    if (this.events[eventKey] && this.events[eventKey] instanceof Function) {
      (this.events[eventKey] as Function).call(null, payload);
    }
  }

  @observable issue: Issue = {} as Issue;

  async load(outside: boolean = this.outside, organizationId?: string) {
    const issueId = this.selected;
    if (issueId) {
      this.loading = true;
      try {
        const issue = await issueApi.project(this.projectId).load(issueId);
        runInAction(() => {
          this.initApi(outside, organizationId);
          this.issue = issue;
          this.loading = false;
        });
      } catch (error) {
        this.loading = false;
      }
    }
  }

  @action
  async getLogs(backlogId?: number) {
    if (!this.outside) {
      const logs = await dataLogApi.project(this.projectId).loadByIssue(backlogId || this.selected);
      this.setLogs(logs);
    }
    return true;
  }

  @action
  async getCustomFields(backlogId?: number, typeId?: string) {
    const customFields = await fieldApi
      .project(this.projectId)
      .org(this.organizationId)
      .getFieldAndValue(backlogId || this.selected, {
        schemeCode: 'agile_issue',
        issueTypeId: typeId || this.issue.issueTypeId as string,
        pageCode: 'agile_issue_edit',
      });
    this.setCustomFields(customFields);
    return true;
  }

  async refresh() {
    await this.load();
    await this.getLogs();
    await this.getCustomFields();
  }

  @action
  async update(field: string, value: any) {
    // const data: DemandUpdate = {
    //   id: this.issue.id,
    //   objectVersionNumber: this.issue.objectVersionNumber,
    // };
    // switch (field) {
    //   case 'description':
    //   case 'feedback': {
    //     await uploadAndReplaceImage(value, this.outside, this.projectId);
    //     data[field] = JSON.stringify(value);
    //     break;
    //   }
    //   case 'assignees': {
    //     data.assignee = Array.isArray(value) ? value : new Array(value || 0);
    //     break;
    //   }
    //   case 'status': {
    //     try {
    //       this.loading = true;
    //       await demandApi.updateStatus(this.issue.id, value, this.issue.objectVersionNumber);
    //       await this.refresh();
    //       this.fireEvent('update', this.issue);
    //     } catch (error) {
    //       this.loading = false;
    //     }
    //     return;
    //   }
    //   case 'starBeacon': {
    //     try {
    //       this.loading = true;
    //       value ? await demandApi.star(this.issue.id) : await demandApi.unstar(this.issue.id);
    //       await this.refresh();
    //       this.fireEvent('update', this.issue);
    //     } catch (error) {
    //       this.loading = false;
    //     }
    //     return;
    //   }
    //   case 'estimatedStartTime':
    //   case 'estimatedEndTime': {
    //     data[field] = value ? value.format('YYYY-MM-DD HH:mm:ss') : null;
    //     break;
    //   }
    //   default: {
    //     // @ts-ignore
    //     data[field] = value;
    //     break;
    //   }
    // }
    // try {
    //   this.loading = true;
    //   await demandApi.update(data, this.outside, this.projectId);
    //   await this.refresh();
    //   this.fireEvent('update', this.issue);
    // } catch (error) {
    //   this.loading = false;
    //   throw error;
    // }
  }

  @action
  async updateCustomField(field: IField, value: any) {
    // const data: DemandCustomFieldUpdate = {
    //   backlogId: this.issue.id,
    //   fieldId: field.fieldId,
    //   fieldCode: field.fieldCode,
    //   fieldType: field.fieldType,
    //   value,
    // };
    // try {
    //   this.loading = true;
    //   await demandApi.outside(this.outside)
    //     .project(this.projectId)
    //     .org(this.organizationId)
    //     .updateCustomFieldValue(data);
    //   await this.refresh();
    //   this.fireEvent('update', this.issue);
    // } catch (error) {
    //   this.loading = false;
    //   throw error;
    // }
  }

  @action
  async delete() {
    // await demandApi.delete(this.issue.id, this.outside, this.projectId);
    // this.fireEvent('delete', this.issue);
    // this.destroy();
  }

  @action
  async uploadAttachment(formData: FormData) {
    // this.loading = true;
    // try {
    //   await demandApi.uploadAttachment(formData, this.issue.id, this.outside, this.projectId);
    //   this.refresh();
    // } catch (error) {
    //   this.loading = false;
    // }
  }

  @action
  async deleteAttachment(attachmentId: number) {
    // await demandApi.deleteAttachment(attachmentId, this.outside, this.projectId);
    // this.refresh();
  }

  @action
  async createComment(delta: string) {
    // await uploadAndReplaceImage(delta, this.outside, this.projectId);
    // await demandApi.project(this.projectId).createComment(delta, this.issue.id, this.outside, this.projectId);
    // this.refresh();
  }

  @action
  async updateComment(delta: string, comment: IComment) {
    // const { id, objectVersionNumber } = comment;
    // await uploadAndReplaceImage(delta, this.outside, this.projectId);
    // await demandApi.project(this.projectId).updateComment(delta, id, objectVersionNumber, this.outside, this.projectId);
    // this.refresh();
  }

  @action
  async deleteComment(commentId: string) {
    // await demandApi.project(this.projectId).deleteComment(commentId, this.outside, this.projectId);
    // this.refresh();
  }
}
export default Store;
