/* eslint-disable no-param-reassign */
import { observable, action, computed } from 'mobx';
import { stores } from '@choerodon/master';
import {
  find, findIndex, uniq, flatten,
} from 'lodash';
import {
  IField, IIssueType, Issue, User,
} from '@/common/types';
import {
  fieldApi, moveIssueApi, issueApi, userApi, statusTransformApi, IStatusCirculation,
} from '@/api';
import { initTargetIssue, getFinalFields } from './utils';

const { AppState } = stores;
export interface MoveMapItem {
  issue: Issue,
  lostFields: IField[]
  fields: FieldWithValue[],
  target: MoveTarget
}
export interface FieldWithValue {
  fieldId: string,
  fieldType: string,
  fieldCode: string
  projectId: string
  system: boolean
  value: string,
  valueStr: any
}
export interface MoveTargetIssue {
  summary: string
  issueTypeVO: IIssueType
  customFields: Map<string, FieldWithValue>
}
export interface MoveTarget {
  issue: MoveTargetIssue,
  issueTypeId: string,
  fields: IField[]
  statusList: IStatusCirculation[]
  projectId: string
}
class Store {
  @observable loading = false;

  @action setLoading(loading: boolean) {
    this.loading = loading;
  }

  @observable selfFields: IField[] = [];

  @action setSelfFields = (data: IField[]) => {
    this.selfFields = data;
  }

  @observable subTaskFields: IField[] = [];

  @action setSubTaskFields = (data: IField[]) => {
    this.subTaskFields = data;
  }

  @observable subBugFields: IField[] = [];

  @action setSubBugFields = (data: IField[]) => {
    this.subBugFields = data;
  }

  @observable moveToProjectList = [];

  @action setMoveToProjectList = (data: any) => {
    this.moveToProjectList = data;
  }

  subTaskDetailMap = observable.map();

  subBugDetailMap = observable.map();

  @observable subTaskTypeId: string | undefined;

  @action setSubTaskTypeId = (data?: string) => {
    this.subTaskTypeId = data;
  }

  @observable subBugTypeId: string | undefined;

  @action setSubBugTypeId = (data?: string) => {
    this.subBugTypeId = data;
  }

  @observable selectedUserIds: string[] = [];

  @action setSelectUserIds = (data: string[]) => {
    this.selectedUserIds = data;
  }

  @observable selectedUsers: User[] = [];

  @action setSelectedUsers = (data: User[]) => {
    this.selectedUsers = data;
  }

  @observable issueMap = new Map<string, MoveMapItem>();

  @computed get issueMapValues() {
    return [...this.issueMap.values()];
  }

  @computed get issues() {
    return [...this.issueMap.values()].map((t) => t.issue);
  }

  @computed get issueFields() {
    return [...this.issueMap.values()].map((t) => t.fields);
  }

  // 是否所有的必填值都已经填了
  @computed get valueReady() {
    return [...this.issueMap.values()].every((t) => {
      const { target: { fields, issue: { customFields } } } = t;
      const requiredFields = fields.filter((f) => f.required);
      // 每个必选项都可以找到值
      return requiredFields.every((f) => customFields.get(f.fieldCode as string)?.value);
    });
  }

  @action async initIssueMap(issueType: string, mainIssue: Issue) {
    const { subIssueVOList, subBugVOList } = mainIssue;
    const { subTaskTypeId, subBugTypeId } = this;
    // @ts-ignore
    const map = new Map<string, MoveMapItem>([
      [mainIssue.issueId, {
        issue: mainIssue,
        fields: [],
        target: {
          issue: {},
          issueTypeId: issueType,
          fields: [],
        },
      }],
      ...subIssueVOList.map((issue: Issue) => ([issue.issueId, {
        issue,
        fields: [],
        target: {
          issue: {},
          issueTypeId: subTaskTypeId,
          fields: [],
        },
      }])),
      ...subBugVOList.map((issue: Issue) => ([issue.issueId, {
        issue,
        fields: [],
        target: {
          issue: {},
          issueTypeId: subBugTypeId,
          fields: [],
        },
      }])),
    ]);
    this.issueMap = map;
  }

  @observable statusList = []

  @action async loadData(targetIssueTypes: IIssueType[], targetProjectId: string, targetProjectType: string, sourceProjectId: string) {
    const needLoadUserIds = uniq(flatten(this.issues.map((issue) => ([issue.assigneeId, issue.reporterId, issue.mainResponsible?.id])))).filter(Boolean);
    const [users, lostFields, issueDetails, issueFields, targetIssueFields, issueStatus] = await Promise.all([
      Promise.all(
        needLoadUserIds.map((userId) => userApi.project(targetProjectId).getById(userId as string).then((res: any) => res.list[0])),
      ),
      Promise.all(
        this.issueMapValues.map(({ issue, target }) => moveIssueApi.project(sourceProjectId).getFieldsLosed(targetProjectId, issue.issueId, target.issueTypeId)),
      ),
      Promise.all(
        this.issues.map((issue) => issueApi.project(sourceProjectId).load(issue.issueId)),
      ),
      Promise.all(
        this.issues.map((issue) => fieldApi.project(sourceProjectId).getFieldAndValue(issue.issueId, {
          schemeCode: 'agile_issue',
          issueTypeId: issue.issueTypeId,
          pageCode: 'agile_issue_edit',
        })),
      ),
      Promise.all(
        targetIssueTypes.map((issueType) => fieldApi.project(sourceProjectId).getFields({
          issueTypeId: issueType.id,
          pageCode: 'agile_issue_create',
          schemeCode: 'agile_issue',
        }, targetProjectId)),
      ),
      Promise.all(
        targetIssueTypes.map((issueType) => statusTransformApi.project(targetProjectId).loadList(issueType.id)),
      ),
    ]);
    const existUserMap = new Map<string, User>(
      users.filter(Boolean).map((user) => ([user.id, user])),
    );
    issueDetails.forEach((issueDetail, index) => {
      const source = this.issueMap.get(issueDetail.issueId)!;
      source.issue = issueDetail;
      source.fields = issueFields[index];
      source.lostFields = lostFields[index];
      source.target.issue = initTargetIssue(issueDetail);
      source.target.projectId = targetProjectId;

      const issueTypeIndex = findIndex(targetIssueTypes, { id: source.target.issueTypeId });
      const targetFields = targetIssueFields[issueTypeIndex];

      source.target.issue.issueTypeVO = targetIssueTypes[issueTypeIndex];
      const statusList = issueStatus[issueTypeIndex];
      source.target.statusList = statusList;
      // 最后设置fields，保证渲染正确
      source.target.fields = getFinalFields({ fields: targetFields, typeCode: targetIssueTypes[issueTypeIndex].typeCode, targetProjectType });
      // 开始设置一些默认值
      this.setDefaultStatus(statusList, issueDetail, source.target);
      this.setDefaultUserFields(targetProjectId, issueDetail, existUserMap, source.target);
      // 设置相同的字段（来自组织）值，除用户类型的字段外
      issueFields[index].filter((f: any) => !f.system).forEach((f: any) => {
        if (f.value && f.fieldType !== 'member') {
          const sameField = find(targetFields, { fieldId: f.fieldId });
          if (sameField) {
            source.target.issue.customFields.set(sameField.fieldCode, {
              fieldId: sameField.fieldId,
              fieldType: sameField.fieldType,
              fieldCode: sameField.fieldCode as string,
              projectId: sameField.projectId as string,
              system: sameField.system,
              value: f.value,
              valueStr: f.valueStr,
            });
          }
        }
      });
    });
  }

  @action setDefaultStatus(statusList: IStatusCirculation[], issueDetail: Issue, target: MoveTarget) {
    // 判断是否有相同状态
    const sameStatus = find(statusList, { id: issueDetail.statusVO.id });
    const defaultStatus = find(statusList, { defaultStatus: true });
    const status = sameStatus ?? defaultStatus;
    if (status) {
      this.updateFieldValue(status.id, status, 'status', target);
    }
  }

  // 为member类型的字段设置值
  @action setDefaultUserFields(targetProjectId: string, issueDetail: Issue, existUserMap: Map<string, User>, target: MoveTarget) {
    if (issueDetail.assigneeId && existUserMap.has(issueDetail.assigneeId)) {
      const assignee = existUserMap.get(issueDetail.assigneeId)!;
      this.updateFieldValue(assignee.id, assignee, 'assignee', target);
    }
    if (issueDetail.reporterId && existUserMap.has(issueDetail.reporterId)) {
      const reporter = existUserMap.get(issueDetail.reporterId)!;
      this.updateFieldValue(reporter.id, reporter, 'reporter', target);
    } else {
      // 目标项目没有原项目的报告人，就设置为自己
      const self = AppState.userInfo;
      this.updateFieldValue(self.id, self, 'reporter', target);
    }
    if (issueDetail.mainResponsibleUser?.id && existUserMap.has(issueDetail.mainResponsibleUser?.id)) {
      const mainResponsible = existUserMap.get(issueDetail.mainResponsibleUser?.id)!;
      this.updateFieldValue(mainResponsible.id, mainResponsible, 'mainResponsible', target);
    }
  }

  @action
  updateFieldValue(value: string, valueStr: any, fieldCode: string, target: MoveTarget) {
    const field = find(target.fields, { fieldCode });
    if (!field) {
      console.warn(`没有找到field ${fieldCode}`);
      return;
    }
    const currentField = target.issue.customFields.get(fieldCode as string);
    if (currentField) {
      currentField.value = value;
      currentField.valueStr = valueStr;
    } else {
      target.issue.customFields.set(fieldCode, {
        fieldId: field.fieldId,
        fieldType: field.fieldType,
        fieldCode: field.fieldCode as string,
        projectId: field.projectId as string,
        system: field.system,
        value,
        valueStr,
      });
    }
  }
}

const store = new Store();

export default store;
