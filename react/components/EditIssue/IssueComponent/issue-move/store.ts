/* eslint-disable no-param-reassign */
import { observable, action, computed } from 'mobx';
import {
  IField, IIssueType, ILabel, Issue, User,
} from '@/common/types';
import {
  fieldApi, moveIssueApi, issueApi, userApi, statusTransformApi, IStatusCirculation, issueLabelApi,
} from '@/api';
import { find, findIndex } from 'lodash';
import { initTargetIssue, getFinalFields } from './utils';

export interface MoveMapItem {
  issue: Issue,
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
  labelList: ILabel[]
  projectId: string
}
class Store {
  dataMap = observable.map();

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

  @computed get issues() {
    return [...this.issueMap.values()].map((t) => t.issue);
  }

  @computed get issueFields() {
    return [...this.issueMap.values()].map((t) => t.fields);
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

  @action async loadData(targetIssueTypes: IIssueType[], targetProjectId: string, targetProjectType: string) {
    const [issueDetails, issueFields, targetIssueFields, issueStatus, labelList] = await Promise.all([
      Promise.all(
        this.issues.map((issue) => issueApi.load(issue.issueId)),
      ),
      Promise.all(
        this.issues.map((issue) => fieldApi.getFieldAndValue(issue.issueId, {
          schemeCode: 'agile_issue',
          issueTypeId: issue.issueTypeId,
          pageCode: 'agile_issue_edit',
        })),
      ),
      Promise.all(
        targetIssueTypes.map((issueType) => fieldApi.getFields({
          issueTypeId: issueType.id,
          pageCode: 'agile_issue_create',
          schemeCode: 'agile_issue',
        }, targetProjectId)),
      ),
      Promise.all(
        targetIssueTypes.map((issueType) => statusTransformApi.project(targetProjectId).loadList(issueType.id)),
      ),
      issueLabelApi.loads(targetProjectId),
    ]);
    // const needSetUserMap=new Map();
    issueDetails.forEach((issueDetail, index) => {
      const source = this.issueMap.get(issueDetail.issueId)!;
      source.issue = issueDetail;
      source.fields = issueFields[index];
      source.target.issue = initTargetIssue(issueDetail);
      source.target.projectId = targetProjectId;

      const issueTypeIndex = findIndex(targetIssueTypes, { id: source.target.issueTypeId });
      const targetFields = targetIssueFields[issueTypeIndex];

      source.target.issue.issueTypeVO = targetIssueTypes[issueTypeIndex];
      const statusList = issueStatus[issueTypeIndex];
      source.target.statusList = statusList;
      source.target.labelList = labelList;
      // 最后设置fields，保证渲染正确
      source.target.fields = getFinalFields({ fields: targetFields, typeCode: targetIssueTypes[issueTypeIndex].typeCode, targetProjectType });
      // 开始设置一些默认值
      this.setDefaultStatus(statusList, issueDetail, source.target);
      // needSetUserMap.set()
    });
    // await this.setDefaultUserFields(targetProjectId, issueDetail, target);
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

  @action async setDefaultUserFields(targetProjectId: string, issueDetail: Issue, target: MoveTarget) {
    // 判断是否有相同状态
    const [assignee, reporter, mainResponsible] = await Promise.all([
      issueDetail.assigneeId && userApi.project(targetProjectId).getById(issueDetail.assigneeId),
      issueDetail.reporterId && userApi.project(targetProjectId).getById(issueDetail.reporterId),
      issueDetail.mainResponsible?.id && userApi.project(targetProjectId).getById(issueDetail.mainResponsible?.id),
    ]);

    if (assignee) {
      this.updateFieldValue(assignee.id, assignee, 'assignee', target);
    }
    if (reporter) {
      this.updateFieldValue(reporter.id, reporter, 'reporter', target);
    } else {
      // 目标项目没有原项目的报告人，就设置为自己
      const self = await userApi.getSelf();
      this.updateFieldValue(self.id, self, 'reporter', target);
    }
    if (mainResponsible) {
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
