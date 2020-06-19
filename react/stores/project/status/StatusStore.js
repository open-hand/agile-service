import {
  observable, action, computed, toJS, 
} from 'mobx';
import { store, stores } from '@choerodon/boot';

const { AppState } = stores;

@store('StatusStore')
class StatusStore {
    @observable statusList = [];
    
  @computed get getStatusList() {
      return toJS(this.statusList);
    }

  @action setStatusList(data) {
    this.statusList = data;
  }
}

const statusStore = new StatusStore();
export default statusStore;
