import {
  observable, action, computed, toJS,
} from 'mobx';
import {
  every,
  filter, find, includes, map, castArray, reverse,
} from 'lodash';
import { Choerodon, stores } from '@choerodon/boot';
import { issueApi, uiApi } from '@/api';
import { getProjectId } from '@/utils/common';

const hiddenFields = ['issueType', 'summary', 'description', 'remainingTime', 'storyPoints', 'estimateTime', 'copingStrategy'];
const copyHiddenFields = ['issueType', 'summary', 'description', 'timeTrace', 'creationDate', 'lastUpdateDate', 'created_user', 'last_updated_user', 'epicName'];
const pageCascadeFields = ['component', 'priority', 'fixVersion', 'influenceVersion'];
const ruleControlSystemFields = ['component', 'priority', 'fixVersion', 'influenceVersion', 'assignee', 'reporter', 'mainResponsible', 'estimatedStartTime', 'estimatedEndTime', 'benfitHypothesis', 'acceptanceCritera', 'subProject'];
function isMultiple(field) {
  return field.fieldType === 'multiple' || field.fieldType === 'checkbox' || field.fieldType === 'multiMember';
}
function isSelect(field) {
  return includes(['radio', 'multiple', 'checkbox', 'single'], field.fieldType);
}

function getValue(issue, field) {
  switch (field.fieldCode) {
    case 'influenceVersion': {
      const { versionIssueRelVOList = [] } = issue;
      const influenceVersions = filter(versionIssueRelVOList, { relationType: 'influence' }) || [];
      return map(toJS(influenceVersions), 'versionId');
    }
    case 'fixVersion': {
      const { versionIssueRelVOList = [] } = issue;
      const fixVersionsTotal = filter(versionIssueRelVOList, { relationType: 'fix' }) || [];
      const fixVersions = filter(fixVersionsTotal, (v) => v.statusCode !== 'archived') || [];
      return map(toJS(fixVersions), 'versionId');
    }
    case 'priority': {
      return toJS(issue.priorityId);
    }
    case 'component': {
      return map(toJS(issue.componentIssueRelVOList) || [], 'componentId');
    }
    default: {
      return toJS(field.value);
    }
  }
}
const { AppState } = stores;

const hasValue = (issue, field) => (isMultiple(field) ? getValue(issue, field)?.length : getValue(issue, field));

class EditIssueStore {
  events = { updateAfter: () => { }, updateBefore: () => { } };

  constructor({ projectId }) {
    this.projectId = projectId;
  }

  // issue
  @observable issue = {};

  @action setIssue(data) {
    this.issue = data;
  }

  @computed get getIssue() {
    return this.issue;
  }

  @computed get issueProjectCategories() {
    return !this.getIssue.projectVO?.id || String(AppState.menuType?.id) === String(this.issue.projectVO.id) ? AppState.menuType?.categories || [] : this.issue.projectVO?.categories || AppState.menuType.categories || [];
  }

  // fields
  @observable fields = [];

  // 存触发器触发的其他字段改变的信息
  @observable updateMessage = {};

  @action setUpdateMessage = (updateMessage) => {
    this.updateMessage = updateMessage;
  }

  @observable updateFieldsAndValue = [];

  @action setUpdateFieldsAndValue = (data) => {
    this.updateFieldsAndValue = data;
  }

  @action setIssueFields(issue, fields) {
    const newFields = fields.map((item) => {
      const newFieldAndValue = reverse(this.updateFieldsAndValue || []).find((field) => field.fieldId === item.fieldId);
      if (newFieldAndValue) {
        return newFieldAndValue;
      }
      return item;
    });
    this.fields = newFields;
    this.issue = { ...issue, ...this.updateMessage };
    this.updateMessage = {};
    this.updateFieldsAndValue = [];
  }

  getFieldByCode(code) {
    return find(this.fields, { fieldCode: code });
  }

  @computed get customFields() {
    return this.fields.filter((field) => !hiddenFields.includes(field?.fieldCode));
  }

  @computed get copyFields() {
    return [...(this.issue.typeCode === 'story' ? [{
      system: true, fieldCode: 'storyPoints', fieldName: '故事点', fieldType: 'number',
    }] : []), ...(this.issue.typeCode === 'task' || this.issue.typeCode === 'sub_task' || this.issue.typeCode === 'bug' ? [{
      system: true, fieldCode: 'remainingTime', fieldName: '剩余预估时间', fieldType: 'number',
    }] : []), ...this.fields.filter((field) => !copyHiddenFields.includes(field?.fieldCode))];
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

  @action initIssueAttribute(doc, workLogs, dataLogs, linkIssues, comments) {
    this.doc = doc;
    this.workLogs = workLogs || [];
    this.dataLogs = dataLogs || [];
    this.linkIssues = linkIssues || [];
    this.comments = comments;
  }

  @observable createBranchShow = false;

  @observable linkBranchShow = false;

  @observable commitShow = false;

  @observable mergeRequestShow = false;

  @observable workLogShow = false;

  @observable copyIssueShow = false;

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

  @action setCopyIssueShow(data) {
    this.copyIssueShow = data;
  }

  @computed get getCopyIssueShow() {
    return this.copyIssueShow;
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

  @observable comments;

  @action setComments = (comments) => {
    this.comments = comments;
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

  @computed get isCurrentProject() {
    return this.outside ? false : getProjectId() && this.projectId && String(getProjectId()) === String(this.projectId);
  }

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

  // 更新的话，重新加载数据有没有加载完
  @observable updateLoaded = false;

  @action setUpdateLoaded = (loaded) => {
    this.updateLoaded = loaded;
  }

  async update(data, ignoreEvents = []) {
    ignoreEvents.includes('updateBefore') || await this.events.updateBefore();
    let res;
    try {
      res = await issueApi.project(this.projectId).update(data);
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

  @observable issueTypeRules = [];

  @action setIssueTypeRules = (rules) => {
    this.issueTypeRules = rules;
  }

  @computed get getAllRules() {
    let allRules = [];
    this.fields?.forEach((field) => {
      if (((field.system && includes(pageCascadeFields, field.fieldCode)) || (!field.system && isSelect(field))) && hasValue(this.issue, field)) {
        allRules = [...allRules, ...filter(this.issueTypeRules, (rule) => rule.fieldId === field.fieldId && includes(castArray(getValue(this.issue, field)), rule.fieldOptionId))];
      }
    });
    return allRules;
  }

  @action getRuleRequired(field) {
    const fieldRules = filter(this.getAllRules, { cascadeFieldCode: field?.fieldCode });
    return !!find(fieldRules, { required: true });
  }

  @action getOptionsData(field, currentValue) {
    const fieldRules = filter(this.getAllRules, { cascadeFieldCode: field?.fieldCode });
    const ruleIds = fieldRules.length ? map(fieldRules, 'id') : undefined;
    return ({
      fieldId: field.fieldId,
      extendParams: field?.fieldCode === 'fixVersion' ? ['sprint_planning', 'started'] : undefined,
      ruleIds,
      // eslint-disable-next-line no-nested-ternary
      selected: ruleIds?.length && currentValue ? (isMultiple(field) ? currentValue : [currentValue]) : undefined,
    });
  }

  @action getRuleHidden(field) {
    const fieldRules = filter(this.getAllRules, { cascadeFieldCode: field?.fieldCode });
    if (field) {
      if (field.required || find(fieldRules, { required: true }) || every(fieldRules, (rule) => !rule.hidden)) {
        return false;
      }
      return true;
    }
    return true;
  }

  @action getRuleHiddenFields() {
    const ruleHiddenFields = [];
    const ruleControlFields = this.customFields.filter((item) => ((item.system && includes(ruleControlSystemFields, item.fieldCode)) || !item.system));
    ruleControlFields.forEach((field) => {
      if (this.getRuleHidden(field)) {
        ruleHiddenFields.push(field?.fieldCode);
      }
    });
  }

  @observable notAllowedTransferStatus = [];

  @computed get getNotAllowedTransferStatus() {
    return this.notAllowedTransferStatus;
  }

  @action setNotAllowedTransferStatus(data) {
    this.notAllowedTransferStatus = data || [];
  }
}
export default EditIssueStore;
