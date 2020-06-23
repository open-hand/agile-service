import { store } from '@choerodon/boot';
import {
  observable, action, computed, toJS, 
} from 'mobx';
import { sprintApi } from '@/api';


@store('BurndownChartStore')
class BurndownChartStore {
  @observable burndownList = [];

  @observable sprintList = [];

  @observable burndownCoordinate = {}

  @computed get getBurndownCoordinate() {
    return toJS(this.burndownCoordinate);
  }

  @action setBurndownCoordinate(data) {
    this.burndownCoordinate = data;
  }

  @computed get getSprintList() {
    return toJS(this.sprintList);
  }

  @action setSprintList(data) {
    this.sprintList = data;
  }
    
  @computed get getBurndownList() {
    return toJS(this.burndownList);
  }

  @action setBurndownList(data) {
    this.burndownList = data;
  }

  axiosGetSprintList() {
    return sprintApi.loadSprints(['started', 'closed']);
  }
}

const burndownChartStore = new BurndownChartStore();
export default burndownChartStore;
