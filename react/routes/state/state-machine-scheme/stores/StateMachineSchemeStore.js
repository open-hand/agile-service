import { observable, action, computed } from 'mobx';
import { Choerodon } from '@choerodon/boot';
import querystring from 'query-string';
import { stateMachineSchemeApi, issueTypeApi, stateMachineApi } from '@/api';

class StateMachineSchemeStore {
  @observable isLoading = false;

  @observable stateMachineLoading = false;

  @observable stateMachineSchemeList = [];

  @observable pagination = {
    current: 1,
    pageSize: 10,
  };

  @observable isAddVisible = false;

  @observable graphLoading = false;

  @observable isConnectVisible = false;

  @observable isMachineDeleteVisible = false;

  @observable isSchemeDeleteVisible = false;

  @observable isPublishVisible = false;

  @observable stateMachine = {};

  @observable allStateMachine = [];

  @observable allIssueType = [];

  @observable newStateMachineId = '';

  @observable schemeVOS = [];

  @observable selectedIssueTypeId = [];

  @observable nodeData = [];

  @observable transferData = [];

  @observable publishData = [];

  @observable publishLoading = false;


  @action setGraphLoading(data) {
    this.graphLoading = data;
  }

  @computed
  get getGraphLoading() {
    return this.graphLoading;
  }

  @computed
  get getIsLoading() {
    return this.isLoading;
  }

  @action
  setIsLoading(loading) {
    this.isLoading = loading;
  }

  @computed
  get getStateMachineLoading() {
    return this.stateMachineLoading;
  }

  @action
  setStateMachineLoading(loading) {
    this.stateMachineLoading = loading;
  }

  @computed
  get getStateMachineSchemeList() {
    return this.stateMachineSchemeList.slice();
  }

  @action
  setStateMachineSchemeList(data) {
    this.stateMachineSchemeList = data;
  }

  @computed
  get getStateMachine() {
    return this.stateMachine;
  }

  @action
  setStateMachine(data) {
    this.stateMachine = data;
  }

  @computed
  get getIsAddVisible() {
    return this.isAddVisible;
  }

  @action
  setIsAddVisible(visibleStatus) {
    this.isAddVisible = visibleStatus;
  }

  @computed
  get getIsConnectVisible() {
    return this.isConnectVisible;
  }

  @action
  setIsConnectVisible(visibleStatus) {
    this.isConnectVisible = visibleStatus;
  }

  @computed
  get getIsPublishVisible() {
    return this.isPublishVisible;
  }

  @action
  setIsPublishVisible(data) {
    this.isPublishVisible = data;
  }

  @computed
  get getIsMachineDeleteVisible() {
    return this.isMachineDeleteVisible;
  }

  @action
  setIsMachineDeleteVisible(visibleStatus) {
    this.isMachineDeleteVisible = visibleStatus;
  }

  @computed
  get getIsSchemeDeleteVisible() {
    return this.isSchemeDeleteVisible;
  }

  @action
  setIsSchemeDeleteVisible(visibleStatus) {
    this.isSchemeDeleteVisible = visibleStatus;
  }

  @computed
  get getAllStateMachine() {
    return this.allStateMachine;
  }

  @action
  setAllStateMachine(data) {
    this.allStateMachine = data;
  }

  @computed
  get getAllIssueType() {
    return this.allIssueType;
  }

  @action
  setAllIssueType(data) {
    this.allIssueType = data;
  }

  @computed
  get getSchemeVOS() {
    return this.schemeVOS;
  }

  @action
  setSchemeVOS(data) {
    this.schemeVOS = data;
  }

  @computed
  get getNewStateMachineId() {
    return this.newStateMachineId;
  }

  @action
  setNewStateMachineId(data) {
    this.newStateMachineId = data;
  }

  @computed
  get getSelectedIssueTypeId() {
    return this.selectedIssueTypeId;
  }

  @action
  setSelectedIssueTypeId(data) {
    this.selectedIssueTypeId = data;
  }

  @computed
  get getNodeData() {
    return this.nodeData;
  }

  @action
  setNodeData(data) {
    this.nodeData = data;
  }

  @computed
  get getTransferData() {
    return this.transferData;
  }

  @action
  setTransferData(data) {
    this.transferData = data;
  }

  @computed
  get getPublishData() {
    return this.publishData;
  }

  @action
  setPublishData(data) {
    this.publishData = data;
  }

  @action setPublishLoading(data) {
    this.publishLoading = data;
  }

  @computed
  get getPublishLoading() {
    return this.publishLoading;
  }

  loadStateMachineSchemeList = (orgId, pagination = this.pagination, sort = { field: 'id', order: 'desc' }, map = {}) => {
    this.setIsLoading(true);
    const { current, pageSize } = pagination;
    return stateMachineSchemeApi.loadList(current, pageSize, `${sort.field},${sort.order}`, querystring.parse(querystring.stringify(map))).then(
      action((data) => {
        this.setIsLoading(false);
        if (data && data.failed) {
          return Promise.reject(data.message);
        } else {
          this.setStateMachineSchemeList(data.list);
          this.pagination = {
            ...pagination,
            total: data.total,
          };
          return Promise.resolve(data);
        }
      }),
    )
      .catch((err) => {
        this.setIsLoading(false);
        return Promise.reject(err);
      });
  };

  createStateMachineScheme = (stateMachineScheme, organizationId) => stateMachineSchemeApi.create(stateMachineScheme)
    .then(
      action(() => {
        this.loadStateMachineSchemeList(organizationId);
      }),
    )
    .catch(() => {
      Choerodon.prompt('保存失败');
    });

  loadStateMachine = (orgId, schemeId, isDraft = true) => {
    this.setStateMachineLoading(true);
    return stateMachineSchemeApi.load(schemeId, isDraft).then(
      action((data) => {
        if (data && data.failed) {
          return Promise.reject(data.message);
        } else {
          this.setStateMachineLoading(false);
          this.setStateMachine(data || {});
          return Promise.resolve(data);
        }
      }),
    );
  };


  loadAllStateMachine = () => stateMachineApi.loadAll().then(
    action((res) => {
      this.setAllStateMachine(res);
    }),
  );

  loadAllIssueType(orgId, schemeId) {
    return issueTypeApi.loadAllByScheme(schemeId).then(
      action((res) => {
        this.setAllIssueType(res);
      }),
    );
  }

  // 检查发布
  checkPublishStateMachine = (orgId, schemeId) => stateMachineSchemeApi.checkPublish(schemeId).then((data) => {
    if (data) {
      this.setPublishData(data);
      this.setPublishLoading(false);
    } else {
      this.setPublishData([]);
      this.setPublishLoading(false);
    }
  }).catch(() => {
    this.setPublishData([]);
    this.setPublishLoading(false);
  });


  handleProptError = (error) => {
    if (error && error.failed) {
      // Choerodon.prompt(error.message);
      return false;
    } else {
      return error;
    }
  }
}

export default StateMachineSchemeStore;
