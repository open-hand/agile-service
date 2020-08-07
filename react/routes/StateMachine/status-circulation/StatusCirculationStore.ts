import {
  observable, runInAction, action, computed,
} from 'mobx';
import { findIndex, find, pull } from 'lodash';
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
  @observable statusList: IStatusCirculation[] = [{
    id: '1',
    name: '待处理',
    valueCode: 'todo',
    to: ['1', '2'],
    default: true,
  }, {
    id: '2',
    name: '待处理2',
    valueCode: 'doing',
    to: ['1', '2', '3'],
  }, {
    id: '3',
    name: '待处理3',
    valueCode: 'done',
    to: ['1', '2', '3'],
  }, {
    id: '4',
    name: '待处理4',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '5',
    name: '待处理5',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '6',
    name: '待处理6',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '7',
    name: '待处理7',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '8',
    name: '待处理8',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '9',
    name: '待处理9',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '10',
    name: '待处理10',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '11',
    name: '待处理11',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '12',
    name: '待处理12',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '13',
    name: '待处理13',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '14',
    name: '待处理14',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '15',
    name: '待处理15',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '16',
    name: '待处理16',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '17',
    name: '待处理17',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '18',
    name: '待处理18',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '19',
    name: '待处理19',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '20',
    name: '待处理20',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '21',
    name: '待处理21',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '22',
    name: '待处理22',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }];

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
      [to.id]: from.to.includes(to.id),
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
