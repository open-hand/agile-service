import {
  observable, runInAction, action, computed, toJS,
} from 'mobx';
import { findIndex, find } from 'lodash';
import { statusTransformApi, IStatusCirculation, IUpdateTransform } from '@/api';
import { IStatus } from '@/common/types';
import { getIsOrganization } from '@/utils/common';

type ChangeType = 'check' | 'nocheck';

const OPPOSITE: {
  [key in ChangeType]: ChangeType
} = {
  check: 'nocheck',
  nocheck: 'check',
};
interface StatusAction {
  type: ChangeType
  to: IStatus['id']
}
interface StatusAllCheckedItem {
  columnCurrentSize: number
  columnCheckedIds: Set<IStatusCirculation['id']>
  rowChecked: boolean
  rowIndeterminate: boolean
  rowCheckedIds: Set<IStatusCirculation['id']>
  originData: Partial<StatusAllCheckedItem>
}
class StatusCirculationStore {
  @observable statusList: IStatusCirculation[] = [];

  @observable checkedMaps = observable.map<IStatusCirculation['id'], StatusAllCheckedItem>();

  @observable loading = false;

  async getStatusList(issueTypeId: string) {
    this.loading = true;
    try {
      const statusList = await statusTransformApi[getIsOrganization() ? 'orgLoadList' : 'loadList'](issueTypeId);
      runInAction(() => {
        this.statusList = statusList.map((item) => {
          const rowChecked = statusList.length === item.canTransformStatus.length;
          const rowIndeterminate = !rowChecked && item.canTransformStatus.length > 1;
          this.checkedMaps.set(item.id, {
            columnCurrentSize: 0,
            columnCheckedIds: new Set([item.id]),
            rowChecked,
            rowIndeterminate,
            rowCheckedIds: new Set(item.canTransformStatus),
            originData: {
              columnCurrentSize: 0,
              columnCheckedIds: new Set([item.id]),
              rowChecked,
              rowIndeterminate,
              rowCheckedIds: new Set(item.canTransformStatus),
            },
          });
          return item;
        });
        statusList.forEach((item) => {
          item.canTransformStatus.forEach((canStatus) => {
            this.checkedMaps.get(canStatus)!.columnCheckedIds.add(item.id);
            this.checkedMaps.get(canStatus)!.originData.columnCheckedIds!.add(item.id);
            this.checkedMaps.get(canStatus)!.columnCurrentSize += 1;
            this.checkedMaps.get(canStatus)!.originData.columnCurrentSize! += 1;
          });
        });
        this.loading = false;
      });
    } catch (error) {
      this.loading = false;
    }
  }

  @computed
  get data() {
    return this.statusList.map((from) => this.statusList.reduce((result, to) => ({
      ...result,
      ...from,
      [to.id]: from.canTransformStatus.includes(to.id),
    }), {}));
  }

  @action('更改newAction.to列的全选状态')
  changeColumnChecked(record: IStatusCirculation, newAction: StatusAction) {
    if (this.checkedMaps.has(newAction.to)) {
      const columnCheckedInfo = this.checkedMaps.get(newAction.to)!;
      if (newAction.type === 'check') {
        columnCheckedInfo.columnCheckedIds.add(record.id);
      } else {
        columnCheckedInfo.columnCheckedIds.delete(record.id);
      }

      columnCheckedInfo.columnCurrentSize = columnCheckedInfo.columnCheckedIds.size;
    }
  }

  @action('更改record.id行的全选状态')
  changeRowChecked(record: IStatusCirculation, newAction: StatusAction) {
    if (this.checkedMaps.has(record.id)) {
      const rowCheckedInfo = this.checkedMaps.get(record.id)!;
      if (newAction.type === 'check') {
        rowCheckedInfo.rowCheckedIds.add(newAction.to);
      } else {
        rowCheckedInfo.rowCheckedIds.delete(newAction.to);
      }

      rowCheckedInfo.rowChecked = rowCheckedInfo.rowCheckedIds.size === this.statusList.length;
      rowCheckedInfo.rowIndeterminate = rowCheckedInfo.rowCheckedIds.size !== this.statusList.length && rowCheckedInfo.rowCheckedIds.size > 1;
    }
  }

  // 待提交的动作
  @observable actions = new Map<IStatus['id'], StatusAction[]>();

  validActionEffective(status: IStatusCirculation, newAction: StatusAction) {
    const checked = newAction.type === 'check';

    return !(status.canTransformStatus.includes(newAction.to) === checked || this.checkedMaps.get(status.id)?.rowCheckedIds.has(newAction.to) === checked);
  }

  getTransformUpdateData(data: Array<{ id: string, to: string, check: boolean }>) {
    return data.map((item) => {
      const toStatus = this.statusMap.get(item.to)!;
      const fromStatus = this.statusMap.get(item.id)!;
      return {
        startNodeId: fromStatus.nodeId,
        endNodeId: toStatus.nodeId,
        startStatusName: fromStatus.name,
        endStatusName: toStatus.name,
        select: item.check,
      };
    });
  }

  async updateChangeTransform(id: IStatus['id'], to: IStatus['id'], check: boolean, issueTypeId: string) {
    await statusTransformApi[getIsOrganization() ? 'orgBatchUpdate' : 'batchUpdate'](issueTypeId, this.getTransformUpdateData([{ id, to, check }]));
    this.getStatusList(issueTypeId);
  }

  @action
  async checkAllOrUnAll(check: boolean, issueTypeId: string, rowId?: string, colId?: string) {
    this.loading = true;
    const batchUpdateData: Array<{ id: string, to: string, check: boolean }> = [];
    const rowStatus = rowId ? this.checkedMaps.get(rowId) : undefined;
    const colStatus = colId ? this.checkedMaps.get(colId) : undefined;
    if (rowStatus || colStatus) {
      this.statusList.forEach((item) => {
        if (item.id !== rowId && rowStatus && !(rowStatus.rowCheckedIds.has(item.id) === check)) {
          batchUpdateData.push({
            id: rowId!,
            to: item.id,
            check,
          });
        }

        if (item.id !== colId && colStatus && !(colStatus.columnCheckedIds.has(item.id) === check)) {
          batchUpdateData.push({
            id: item.id,
            to: colId!,
            check,
          });
        }
      });
    }
    await statusTransformApi[getIsOrganization() ? 'orgBatchUpdate' : 'batchUpdate'](issueTypeId, this.getTransformUpdateData(batchUpdateData));
    this.getStatusList(issueTypeId);
  }

  @action
  checkChange(id: IStatus['id'], to: IStatus['id'], check: boolean) {
    const status = find(this.statusList, { id });
    if (status) {
      // 可转换
      if (check) {
        this.addAction(id, {
          to,
          type: 'check',
        }, status);
      } else {
        // 不可转换
        this.addAction(id, {
          to,
          type: 'nocheck',
        }, status);
      }
    }
  }

  @action
  addAction(id: IStatus['id'], newAction: StatusAction, status: IStatusCirculation) {
    const { to, type } = newAction;
    let actions = this.actions.get(id);
    if (!actions) {
      this.actions.set(id, []);
      actions = this.actions.get(id);
    }
    if (actions) {
      const oppositeActionIndex = findIndex(actions, {
        to,
        type: OPPOSITE[type],
      });
      // 存在可抵消的就抵消
      if (oppositeActionIndex > -1) {
        this.changeColumnChecked(status, newAction);
        this.changeRowChecked(status, newAction);
        actions.splice(oppositeActionIndex, 1);
      } else if (this.validActionEffective(status, newAction)) {
        this.changeColumnChecked(status, newAction);
        this.changeRowChecked(status, newAction);
        actions.push(newAction);
      }
    }
  }

  @action clearActions() {
    this.actions.clear();
    const newCheckMaps = observable.map<IStatusCirculation['id'], any>();
    this.checkedMaps.forEach((value, key) => {
      newCheckMaps.set(key, {
        ...toJS(value.originData),
        originData: toJS(value.originData),
      });
    });
    this.checkedMaps = newCheckMaps;
  }

  @action clearStatusActions(statusId: IStatus['id']) {
    this.actions.delete(statusId);
  }

  @computed
  get hasAction() {
    for (const actions of this.actions.values()) {
      if (actions && actions.length > 0) {
        return true;
      }
    }
    return false;
  }

  @computed
  get statusMap() {
    return this.statusList.reduce((map, status) => {
      map.set(status.id, status);
      return map;
    }, new Map<IStatus['id'], IStatusCirculation>());
  }

  @computed
  get needSubmitActions(): IUpdateTransform[] {
    const result: IUpdateTransform[] = [];
    const { statusMap } = this;
    for (const [from, actions] of this.actions.entries()) {
      if (actions && actions.length > 0) {
        const fromStatus = statusMap.get(from);
        if (fromStatus) {
          actions.forEach((singleAction) => {
            const { to, type } = singleAction;
            const toStatus = statusMap.get(to);
            if (toStatus) {
              result.push({
                startNodeId: fromStatus.nodeId,
                endNodeId: toStatus.nodeId,
                startStatusName: fromStatus.name,
                endStatusName: toStatus.name,
                select: type === 'check',
              });
            }
          });
        }
      }
    }
    return result;
  }

  async batchUpdateStatusTransform(issueTypeId: string) {
    if (!this.hasAction) {
      return;
    }
    await statusTransformApi[getIsOrganization() ? 'orgBatchUpdate' : 'batchUpdate'](issueTypeId, this.needSubmitActions);
    this.clearActions();
    this.getStatusList(issueTypeId);
  }
}
export default StatusCirculationStore;
