/* eslint-disable no-param-reassign */
import {
  observable, action, runInAction, computed,
} from 'mobx';
import { kanbanTemplateApi, IKanbanTemplateColumn, IKanbanTemplateStatus } from '@/api';
import { DropResult } from 'react-beautiful-dnd';
import { find } from 'lodash';

export default class KanbanTemplateDetailStore {
  constructor(templateId: string) {
    this.templateId = templateId;
  }

  templateId: string

  @observable loading = false

  @observable columns: IKanbanTemplateColumn[] = []

  @observable unsetStatus: IKanbanTemplateStatus[] = []

  @computed get allColumns():IKanbanTemplateColumn[] {
    return [...this.columns, {
      columnId: '0',
      color: 'rgba(0, 0, 0, 0.26)',
      categoryCode: 'todo',
      status: this.unsetStatus,
      boardId: this.templateId,
      name: '未对应的状态',
      sequence: 0,
      objectVersionNumber: 0,
    }];
  }

  @action
  async refresh() {
    this.loading = true;
    if (this.templateId) {
      const res = await kanbanTemplateApi.columns(this.templateId);
      const unsetData = await kanbanTemplateApi.unsetColumn(this.templateId);
      runInAction(() => {
        this.columns = res;
        this.unsetStatus = unsetData.map((status) => ({
          statusId: status.id,
          categoryCode: status.type,
          position: 0,
          templateCompleted: status.complete,
          ...status,
        }));
        this.loading = false;
      });
    }
  }

  async moveStatus(result: DropResult) {
    if (result.source && result.destination) {
      const {
        source: {
          index: sourceIndex,
          droppableId: sourceDroppableId,
        },
        destination: {
          index: destinationIndex,
          droppableId: destinationDroppableId,
        }, draggableId,
      } = result;
      const sourceColumnId = sourceDroppableId;
      const destinationColumnId = destinationDroppableId;
      if (sourceColumnId === destinationColumnId && sourceIndex === destinationIndex) {
        return;
      }
      // 未对应不排序
      if (sourceColumnId === '0' && destinationColumnId === '0') {
        return;
      }
      const { statusId } = JSON.parse(draggableId);
      const sourceColumn = find(this.allColumns, { columnId: sourceColumnId });
      const destinationColumn = find(this.allColumns, { columnId: destinationColumnId });
      const [moved] = sourceColumn?.status.splice(sourceIndex, 1) ?? [];
      if (moved) {
        destinationColumn?.status.splice(destinationIndex, 0, moved);
      }
      this.loading = true;
      if (destinationColumnId === '0') {
        await kanbanTemplateApi.moveStatusToUnset(statusId, {
          columnId: sourceColumnId,
        });
      } else {
        await kanbanTemplateApi.moveStatus(statusId, {
          columnId: destinationColumnId,
          originColumnId: sourceColumnId,
          position: destinationIndex,
        });
      }

      this.refresh();
    }
  }

  async moveColumn(result: DropResult) {
    if (result.source && result.destination) {
      const { source: { index: sourceIndex }, destination: { index: destinationIndex }, draggableId } = result;
      if (sourceIndex !== destinationIndex) {
        const { columnId } = JSON.parse(draggableId);
        const sourceColumn = find(this.columns, { columnId });
        if (sourceColumn) {
          const [moved] = this.columns.splice(sourceIndex, 1) ?? [];
          if (moved) {
            this.columns.splice(destinationIndex, 0, moved);
          }
          this.loading = true;
          await kanbanTemplateApi.moveColumn({
            projectId: 0,
            boardId: sourceColumn.boardId,
            columnId: sourceColumn.columnId,
            sequence: destinationIndex,
            objectVersionNumber: sourceColumn.objectVersionNumber,
          });
          this.refresh();
        }
      }
    }
  }

  @action
  async updateColumnName(column: IKanbanTemplateColumn, newValue: string) {
    this.loading = true;
    await kanbanTemplateApi.updateColumn(column.boardId, column.columnId, {
      projectId: 0,
      boardId: column.boardId,
      columnId: column.columnId,
      name: newValue,
      objectVersionNumber: column.objectVersionNumber,
    });
    this.refresh();
  }

  @action
  async deleteColumn(column: IKanbanTemplateColumn) {
    this.loading = true;
    await kanbanTemplateApi.deleteColumn(column.boardId, column.columnId);
    this.refresh();
  }

  @action
  async setStatusComplete(status: IKanbanTemplateStatus, complete: boolean) {
    await kanbanTemplateApi.updateStatusComplete(status.statusId, complete);
    status.templateCompleted = complete;
  }
}
