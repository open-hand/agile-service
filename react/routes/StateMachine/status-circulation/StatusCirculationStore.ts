import {
  observable, runInAction, action, computed,
} from 'mobx';
import { findIndex, find } from 'lodash';
import { statusTransformApi, IStatusCirculation, IUpdateTransform } from '@/api';
import { IStatus } from '@/common/types';
import takeLast from '@/utils/takeLast';

const loadList = takeLast(statusTransformApi.loadList, statusTransformApi);

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
class StatusCirculationStore {
  @observable statusList: IStatusCirculation[] = [];

  @observable loading = false

  async getStatusList(issueTypeId: string) {
    this.loading = true;
    try {
      const statusList = await loadList(issueTypeId);
      runInAction(() => {
        this.statusList = statusList;
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

  // 待提交的动作
  @observable actions = new Map<IStatus['id'], StatusAction[]>();

  @action
  checkChange(id: IStatus['id'], to: IStatus['id'], check: boolean) {
    const status = find(this.statusList, { id });
    if (status) {
      // 可转换
      if (check) {
        this.addAction(id, {
          to,
          type: 'check',
        });
      } else {
        // 不可转换
        this.addAction(id, {
          to,
          type: 'nocheck',
        });
      }
    }
  }

  @action
  addAction(id: IStatus['id'], newAction: StatusAction) {
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
        actions.splice(oppositeActionIndex, 1);
      } else {
        actions.push(newAction);
      }
    }
  }

  @action clearActions() {
    this.actions.clear();
  }

  @action clearStatusActions(statusId:IStatus['id']) {
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
    await statusTransformApi.batchUpdate(issueTypeId, this.needSubmitActions);
    this.clearActions();
    this.getStatusList(issueTypeId);
  }
}
export default StatusCirculationStore;
