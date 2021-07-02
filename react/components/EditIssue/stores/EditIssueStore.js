import {
  observable, action, computed, toJS,
} from 'mobx';
import { find } from 'lodash';
import { Choerodon } from '@choerodon/boot';
import { issueApi, uiApi } from '@/api';

const hiddenFields = ['issueType', 'summary', 'description', 'remainingTime', 'storyPoints'];
const copyHiddenFields = ['issueType', 'summary', 'description', 'timeTrace', 'creationDate', 'lastUpdateDate', 'created_user', 'last_updated_user', 'epicName'];
class EditIssueStore {
  events = { updateAfter: () => { }, updateBefore: () => { } };

  // issue
  @observable issue = {};

  @action setIssue(data) {
    this.issue = data;
  }

  @computed get getIssue() {
    return this.issue;
  }

  // fields
  @observable fields = [];

  @action setIssueFields(issue, fields) {
    this.fields = fields;
    this.issue = issue;
  }

  getFieldByCode(code) {
    return find(this.fields, { fieldCode: code });
  }

  @computed get customFields() {
    return this.fields.filter((field) => !hiddenFields.includes(field.fieldCode));
  }

  @computed get copyFields() {
    return [...(this.issue.typeCode === 'story' ? [{
      system: true, fieldCode: 'storyPoints', fieldName: '故事点', fieldType: 'number',
    }] : []), ...(this.issue.typeCode === 'task' || this.issue.typeCode === 'sub_task' || this.issue.typeCode === 'bug' ? [{
      system: true, fieldCode: 'remainingTime', fieldName: '剩余预估时间', fieldType: 'number',
    }] : []), ...this.fields.filter((field) => !copyHiddenFields.includes(field.fieldCode))];
  }

  @observable doc = {};

  @observable workLogs = [];

  @observable dataLogs = [];

  @observable linkIssues = [];

  @observable branch = {};

  @action setDoc(data) {
    this.doc = data;
  }

  @computed get getDoc() {
    return this.doc;
  }

  @action setWorkLogs(data) {
    this.workLogs = data;
  }

  @computed get getWorkLogs() {
    return this.workLogs.slice();
  }

  @action setDataLogs(data) {
    this.dataLogs = data;
  }

  @computed get getDataLogs() {
    return this.dataLogs;
  }

  @action setLinkIssues(data) {
    this.linkIssues = data;
  }

  @computed get getLinkIssues() {
    return this.linkIssues;
  }

  @action setBranches(data) {
    this.branches = data;
  }

  @computed get getBranch() {
    return this.branch;
  }

  setBranch(branch) {
    this.branch = branch || {};
  }

  @action initIssueAttribute(doc, workLogs, dataLogs, linkIssues) {
    this.doc = doc;
    this.workLogs = workLogs || [];
    this.dataLogs = dataLogs || [];
    this.linkIssues = linkIssues || [];
  }

  @observable createBranchShow = false;

  @observable linkBranchShow = false;

  @observable commitShow = false;

  @observable mergeRequestShow = false;

  @observable workLogShow = false;

  @observable createSubTaskShow = false;

  @observable createSubBugShow = false;

  @observable copyIssueShow = false;

  @observable transformSubIssueShow = false;

  @observable transformFromSubIssueShow = false;

  @observable assigneeShow = false;

  @observable detailShow = false;

  @observable wsjfDTOShow = true;

  @observable tab = 'detail';

  @action
  setTab(tab) {
    if (tab) {
      this.tab = tab;
    } else {
      this.tab = 'detail';
    }
  }

  @action setCreateBranchShow(data) {
    this.createBranchShow = data;
  }

  @action setLinkBranchShow(data) {
    this.linkBranchShow = data;
  }

  @computed get getCreateBranchShow() {
    return this.createBranchShow;
  }

  @action setCommitShow(data) {
    this.commitShow = data;
  }

  @computed get getCommitShow() {
    return this.commitShow;
  }

  @action setMergeRequestShow(data) {
    this.mergeRequestShow = data;
  }

  @computed get getMergeRequestShow() {
    return this.mergeRequestShow;
  }

  @action setWorkLogShow(data) {
    this.workLogShow = data;
  }

  @computed get getWorkLogShow() {
    return this.workLogShow;
  }

  @action setCreateSubTaskShow(data) {
    this.createSubTaskShow = data;
  }

  @computed get getCreateSubTaskShow() {
    return this.createSubTaskShow;
  }

  @action setCreateSubBugShow(data) {
    this.createSubBugShow = data;
  }

  @computed get getCreateSubBugShow() {
    return this.createSubBugShow;
  }

  @action setCopyIssueShow(data) {
    this.copyIssueShow = data;
  }

  @computed get getCopyIssueShow() {
    return this.copyIssueShow;
  }

  @action setTransformSubIssueShow(data) {
    this.transformSubIssueShow = data;
  }

  @computed get getTransformSubIssueShow() {
    return this.transformSubIssueShow;
  }

  @action setTransformFromSubIssueShow(data) {
    this.transformFromSubIssueShow = data;
  }

  @computed get getTransformFromSubIssueShow() {
    return this.transformFromSubIssueShow;
  }

  @action setAssigneeShow(data) {
    this.assigneeShow = data;
  }

  @computed get getAssigneeShow() {
    return this.assigneeShow;
  }

  @action setDetailShow(detailShow) {
    this.detailShow = detailShow;
  }

  @computed get getDetailShow() {
    return this.detailShow;
  }

  @action setWSJFDTOShow(wsjfDTOShow) {
    this.wsjfDTOShow = wsjfDTOShow;
  }

  @computed get getWSJFDTOShow() {
    return this.wsjfDTOShow;
  }

  @observable backlogLinks = [];

  @action setBacklogLinks = (data) => {
    this.backlogLinks = data;
  }

  commentExpandMap = observable.map();

  commentReplysMap = observable.map();

  promptExtraNodeMap = observable.map();/** 各类提示信息的额外内容 */

  @observable linkedUI = [];

  @action setLinkedUI = (data) => {
    this.linkedUI = data;
  }

  @observable outside = false;

  @observable organizationId;

  @observable projectId;

  /**
   * api初始化， 外部与内部调用的接口在此进行判断
   * @param source
   */
  initApi(outside, organizationId, projectId) {
    this.outside = outside;
    if (this.outside) {
      this.organizationId = organizationId;
    }
    this.projectId = projectId;
  }

  @action
  destroy() {
    this.outside = false;
    this.projectId = undefined;
    this.organizationId = undefined;
  }

  getLinkedUI = () => {
    if (this.issue.issueId) {
      uiApi.project(this.projectId).org(this.organizationId).outside(this.outside).getLinkedUI(this.issue.issueId)
        .then((res) => {
          this.setLinkedUI(res || []);
        });
    }
  }

  refreshBranch = () => { }

  setRefreshBranch(refreshBranch) {
    this.refreshBranch = refreshBranch;
  }

  async update(data, ignoreEvents = []) {
    ignoreEvents.includes('updateBefore') || await this.events.updateBefore();
    let res;
    try {
      res = await issueApi.update(data);
    } catch (error) {
      res = { failed: true, ...error };
      if (res.failed && res.code === 'error.epic.duplicate.feature.summary') {
        Choerodon.prompt('史诗下有相同的特性概要');
      }
    }
    ignoreEvents.includes('updateAfter') || await this.events.updateAfter(res);
    return res;
  }

  @observable defaultTypeId = '';

  @action setDefaultTypeId = (data) => {
    this.defaultTypeId = data;
  }

  @observable defaultSummary = '';

  @action setDefaultSummary = (data) => {
    this.defaultSummary = data;
  }

  @observable defaultSprint = '';

  @action setDefaultSprint = (data) => {
    this.defaultSprint = data;
  }

  @observable defaultAssignee;

  @action setDefaultAssignee = (data) => {
    this.defaultAssignee = data;
  }
}
export default EditIssueStore;
