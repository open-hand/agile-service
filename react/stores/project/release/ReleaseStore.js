import {
  observable, action, computed, toJS,
} from 'mobx';
import { store, stores } from '@choerodon/boot';
import {
  versionApi, priorityApi, statusApi, issueTypeApi, 
} from '@/api';

const { AppState } = stores;

@store('RleaseStore')
class ReleaseStore {
  @observable versionList = [];

  @observable originIssue = [];

  @observable versionDetail = {};

  @observable versionStatusIssues = [];

  @observable publicVersionDetail = {};

  @observable issueTypes = [];

  @observable issueStatus = [];

  @observable issuePriority = [];

  @observable issueCountDetail = {
    todoCount: 0,
    todoStatusCount: 0,
    doingStatusCount: 0,
    doneStatusCount: 0,
    doingCount: 0,
    doingStatus: {},
    doneCount: 0,
    doneStatus: {},
    count: 0,
  };

  @observable filters = {
    advancedSearchArgs: {},
    searchArgs: {},
    content: '',
  };

  @observable filterMap = new Map([
    ['todo', {}],
    ['doing', {}],
    ['done', {}],
    ['0', {}],
  ]);

  @observable deleteReleaseVisible = false;

  @action setSearchContent(data) {
    if (data) {
      this.filters.contents = data;
    }
  }

  @computed get getIssueCountDetail() {
    return this.issueCountDetail;
  }

  @action setIssueCountDetail(data) {
    this.issueCountDetail = data;
  }

  @action setAdvArg(data) {
    if (data) {
      Object.assign(this.filters.advancedSearchArgs, data);
    }
  }

  @action setArg(data) {
    if (data) {
      Object.assign(this.filters.searchArgs, data);
    }
  }

  @computed get getFilter() {
    return toJS(this.filters);
  }

  @action setFilterMap(key) {
    this.filterMap.set(key, toJS(this.filters));
    this.clearArg();
  }

  @computed get getFilterMap() {
    return this.filterMap;
  }

  @action clearArg() {
    this.filters = {
      advancedSearchArgs: {},
      searchArgs: {},
      content: '',
    };
  }

  @computed get getPublicVersionDetail() {
    return toJS(this.publicVersionDetail);
  }

  @action setPublicVersionDetail(data) {
    this.publicVersionDetail = data;
  }

  @computed get getVersionStatusIssues() {
    return toJS(this.versionStatusIssues);
  }

  @action setVersionStatusIssues(data) {
    this.versionStatusIssues = data;
  }

  @computed get getOriginIssue() {
    return toJS(this.originIssue);
  }

  @action setOriginIssue(data) {
    this.originIssue = data;
  }

  @computed get getVersionDetail() {
    return toJS(this.versionDetail);
  }

  @action setVersionDetail(data) {
    this.versionDetail = data;
  }

  @computed get getVersionList() {
    return toJS(this.versionList);
  }

  @action setVersionList(data) {
    this.versionList = data;
  }

  @action setFilters(data) {
    this.filters = data;
  }

  @action setIssueTypes(data) {
    this.issueTypes = data;
  }

  @computed get getIssueTypes() {
    return toJS(this.issueTypes);
  }

  @action setIssuePriority(data) {
    this.issuePriority = data;
  }

  @computed get getIssuePriority() {
    return toJS(this.issuePriority);
  }

  @action setIssueStatus(data) {
    this.issueStatus = data;
  }

  @computed get getIssueStatus() {
    return toJS(this.issueStatus);
  }

  @action setDeleteReleaseVisible(data) {
    this.deleteReleaseVisible = data;
  }

  @computed get getDeleteReleaseVisible() {
    return this.deleteReleaseVisible;
  }

  axiosGetVersionList(pageRequest) {
    return versionApi.loadVersionList(pageRequest.page, pageRequest.size, this.filters);
  }

  async getSettings() {
    const type = await issueTypeApi.loadAll();
    this.setIssueTypes(type);

    const status = await statusApi.loadByProject();
    this.setIssueStatus(status);

    const priorities = await priorityApi.loadByProject();
    this.setIssuePriority(priorities);
  }
}

const releaseStore = new ReleaseStore();
export default releaseStore;
