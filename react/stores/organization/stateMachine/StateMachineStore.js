import { observable, action, computed } from 'mobx';
import { axios, store, Choerodon } from '@choerodon/boot';
import querystring from 'query-string';
import { stateMachineApi } from '@/api';

@store('StateMachineStore')
class StateMachineStore {
  @observable isLoading = false;

  @observable stateMachine = {};

  @observable configType = 'config_condition';

  @computed get getIsLoading() {
    return this.isLoading;
  }

  @computed get getConfigType() {
    return this.configType;
  }

  @computed get getStateMachine() {
    return this.stateMachine;
  }

  @action setConfigType(type) {
    this.configType = type;
  }

  @action setIsLoading(loading) {
    this.isLoading = loading;
  }

  @action setStateMachine(data) {
    this.stateMachine = data;
  }

  loadStateMachineList = (sort = { field: 'id', order: 'desc' }, map = {}) => {
    this.setIsLoading(true);
    return stateMachineApi.loadList(map, sort).then((data) => {
      // this.setStateList(data);
      this.setIsLoading(false);
      if (data && data.failed) {
        Choerodon.propmt(data.message);
      } else {
        return Promise.resolve(data);
      }
      return '';
    });
  }

  loadStateMachineDeployById = (orgId, stateId) => stateMachineApi.loadWithConfig(stateId).then((data) => {
    const res = this.handleProptError(data);
    if (data) {
      this.setStateMachine(data);
    }
    return res;
  });

  loadStateMachineDraftById = (orgId, stateId) => stateMachineApi.loadWithDraftConfig(stateId).then((data) => {
    const res = this.handleProptError(data);
    if (data) {
      this.setStateMachine(data);
    }
    return res;
  });


  loadTransferConfigList = (orgId, id, type) => {
    this.setIsLoading(true);
    return stateMachineApi.loadTransferConfig(id, type)
      .then((data) => {
        this.setIsLoading(false);
        return this.handleProptError(data);
      });
  };


  handleProptError = (error) => {
    if (error && error.failed) {
      Choerodon.prompt(error.message);
      return false;
    } else {
      return error;
    }
  }
}

const stateMachineStore = new StateMachineStore();
export default stateMachineStore;
