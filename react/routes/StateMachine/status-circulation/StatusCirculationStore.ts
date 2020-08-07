import {
  observable, runInAction, action, computed,
} from 'mobx';
import { findIndex, find } from 'lodash';
import { statusTransformApi, IStatusCirculation } from '@/api';
import { IStatus } from '@/common/types';

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
    const statusList = await statusTransformApi.loadList(issueTypeId);
    runInAction(() => {
      this.statusList = statusList;
      this.loading = false;
    });
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

  @computed
  get hasAction() {
    for (const actions of this.actions.values()) {
      if (actions && actions.length > 0) {
        return true;
      }
    }
    return false;
  }
}
export default StatusCirculationStore;
