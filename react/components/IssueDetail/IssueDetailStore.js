import { observable, action, computed } from 'mobx';
import {
  loadBranchs, loadDatalogs, loadLinkIssues,
  loadIssue, loadWorklogs, loadDocs, getFieldAndValue, 
} from '@/api/NewIssueApi';
import {
  loadDatalogs as loadDatalogsProgram,
  loadIssue as loadIssueProgram,
  getFieldAndValue as getFieldAndValueProgram,
} from '@/api/QueryProgramApi';

class IssueDetailStore {
  @observable loading = false;

  @action setLoading(loading) {
    this.loading = loading;
  }

  @observable issue = {};

  @action setIssue(data) {
    this.issue = data;
  }

  @computed get getIssue() {
    return this.issue;
  }

  @observable fields = [];

  @action setIssueFields(issue, fields) {
    this.fields = fields;
    this.issue = issue;
  }

  @computed get getFields() {
    return this.fields;
  }

  loadIssueDetail = async (paramIssueId) => {
    this.setLoading(true);
    try {
      // 1. 加载详情
      const issue = await (programId ? loadIssueProgram(id, programId) : loadIssue(id));
      if (idRef.current !== id) {
        return;
      }
      
      // 2. 根据详情加载fields
      const param = {
        schemeCode: 'agile_issue',
        context: issue.typeCode,
        pageCode: 'agile_issue_edit',
      };
      const fields = await (programId ? getFieldAndValueProgram(id, param, programId) : getFieldAndValue(id, param));
      setIssueLoading(false);
      store.setIssueFields(issue, fields);
      
      // 3. 加载额外信息
      const [
        doc,
        workLogs,
        dataLogs,
        linkIssues,
        branches,
      ] = await Promise.all([
        loadDocs(id),
        programId || applyType === 'program' ? null : loadWorklogs(id),
        programId ? loadDatalogsProgram(id, programId) : loadDatalogs(id),
        programId || applyType === 'program' ? null : loadLinkIssues(id),
        programId || applyType === 'program' ? null : loadBranchs(id),
      ]);
      if (idRef.current !== id) {
        return;
      }
      store.initIssueAttribute(doc, workLogs, dataLogs, linkIssues, branches, []);
    } catch (error) {
      Choerodon.prompt(error.message, 'error');
    }
  };

  @observable issueTypes = [];

  // issue attribute
  @observable doc = {};

  @observable workLogs = [];

  @observable dataLogs = [];

  @observable linkIssues = [];

  @observable branch = {};

  @observable testExecutes = [];

  @action setIssueTypes(issueTypes) {
    this.issueTypes = issueTypes;
  }

  @computed get getIssueTypes() {
    return this.issueTypes;
  }

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

  @action initIssueAttribute(doc, workLogs, dataLogs, linkIssues, branch) {
    this.doc = doc;
    this.workLogs = workLogs;
    this.dataLogs = dataLogs;
    this.linkIssues = linkIssues || [];
    this.branch = branch || {};
  }


  @observable createBranchShow = false;

  @observable commitShow = false;

  @observable mergeRequestShow = false;

  @observable workLogShow = false;

  @observable createSubTaskShow = false;

  @observable createSubBugShow = false;

  @observable copyIssueShow = false;

  @observable transformSubIssueShow = false;

  @observable transformFromSubIssueShow = false;

  @observable relateStoryShow = false;

  @observable assigneeShow = false;

  @observable changeParentShow = false;

  @observable detailShow = false;

  @observable wsjfDTOShow = true;

  @action setCreateBranchShow(data) {
    this.createBranchShow = data;
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

  @action setRelateStoryShow(data) {
    this.relateStoryShow = data;
  }

  @computed get getRelateStoryShow() {
    return this.relateStoryShow;
  }

  @action setAssigneeShow(data) {
    this.assigneeShow = data;
  }

  @computed get getAssigneeShow() {
    return this.assigneeShow;
  }

  @action setChangeParentShow(data) {
    this.changeParentShow = data;
  }

  @computed get getChangeParentShow() {
    return this.changeParentShow;
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
}
export default IssueDetailStore;
