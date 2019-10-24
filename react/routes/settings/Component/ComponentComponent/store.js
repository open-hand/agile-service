import { observable, computed, action } from 'mobx';

export default class Store {
  @observable defaultAssigneeRole = '';

  @observable description = '';

  @observable managerId = '';

  @observable name = '';

  @computed
  get getDefaultAssigneeRole() {
    return this.defaultAssigneeRole;
  }

  @action
  setDefaultAssigneeRole(data) {
    this.defaultAssigneeRole = data;
  }

  @computed
  get getDescription() {
    return this.description;
  }

  @action
  setDescription(data) {
    this.description = data;
  }

  @computed
  get getManagerId() {
    return this.managerId;
  }

  @action
  setManagerId(data) {
    this.managerId = data;
  }

  @computed
  get getName() {
    return this.name;
  }

  @action
  setName(data) {
    this.name = data;
  }
}
