/* eslint-disable camelcase */
import {
  observable, action, computed, toJS,
} from 'mobx';
import { includes, omit, sortBy } from 'lodash';
import { has } from '@choerodon/inject';
import { isInProgram } from '@/utils/program';

export function getSystemFields(excludeCodes = []) {
  const systemFields = [{
    code: 'issueIds',
    name: 'issueId',
    defaultShow: true,
    noDisplay: true, // 不需要展示，仅作为一个筛选项
  }, {
    code: 'starBeacon',
    name: 'starBeacon',
    defaultShow: true,
    noDisplay: true, // 不需要展示，仅作为一个筛选项
  }, {
    code: 'myAssigned',
    name: 'myAssigned',
    defaultShow: true,
    noDisplay: true, // 不需要展示，仅作为一个筛选项
  }, {
    code: 'userId',
    name: 'userId',
    defaultShow: true,
    noDisplay: true, // 不需要展示，仅作为一个筛选项
  }, {
    code: 'quickFilterIds',
    name: '快速筛选',
    nameKey: 'agile.systemField.quickFilter',
    defaultShow: true,
    noDisplay: true,
  }, {
    code: 'contents',
    name: '概要',
    nameKey: 'agile.systemField.summary',
    defaultShow: true,
    noDisplay: true,
  }, {
    code: 'issueTypeId',
    name: '工作项类型',
    nameKey: 'agile.systemField.issueType',
    defaultShow: true,
    fieldType: 'multiple',
  }, {
    code: 'statusId',
    name: '状态',
    nameKey: 'agile.systemField.status',
    defaultShow: true,
    fieldType: 'multiple',
  }, {
    code: 'assigneeId',
    name: '经办人',
    nameKey: 'agile.systemField.assignee',
    defaultShow: true,
    fieldType: 'member',
  }, {
    code: 'reporterIds',
    name: '报告人',
    nameKey: 'agile.systemField.reporter',
    defaultShow: false,
    fieldType: 'member',
  }, {
    code: 'sprint',
    name: '冲刺',
    nameKey: 'agile.systemField.sprint',
    defaultShow: true,
    fieldType: 'multiple',
  }, {
    code: 'component',
    name: '模块',
    nameKey: 'agile.systemField.component',
    defaultShow: false,
    fieldType: 'multiple',
  }, {
    code: 'label',
    name: '标签',
    nameKey: 'agile.systemField.label',
    defaultShow: false,
    fieldType: 'multiple',
  }, {
    code: 'priorityId',
    name: '优先级',
    nameKey: 'agile.systemField.priority',
    defaultShow: true,
    fieldType: 'multiple',
  }, {
    code: 'version',
    name: '版本',
    nameKey: 'agile.systemField.version',
    // 不可选择，这个字段被废弃了，但是可以展示
    archive: true,
    fieldType: 'multiple',
  }, {
    code: 'fixVersion',
    name: '修复的版本',
    nameKey: 'agile.systemField.fixVersion',
    fieldType: 'multiple',
  }, {
    code: 'influenceVersion',
    name: '影响的版本',
    nameKey: 'agile.systemField.influenceVersion',
    fieldType: 'multiple',
  }, {
    code: 'epic',
    name: '史诗',
    nameKey: 'agile.systemField.epic',
    defaultShow: !isInProgram(),
    fieldType: 'multiple',
  }, {
    code: 'feature',
    name: '特性',
    nameKey: 'agile.systemField.feature',
    defaultShow: true,
    fieldType: 'multiple',
  }, {
    code: 'createDate',
    name: '创建时间',
    nameKey: 'agile.systemField.createDate',
    defaultShow: false,
    fieldType: 'datetime',
  }, {
    code: 'updateDate',
    name: '更新时间',
    nameKey: 'agile.systemField.updateDate',
    defaultShow: false,
    fieldType: 'datetime',
  },
  {
    code: 'estimatedStartTime',
    name: '预计开始时间',
    nameKey: 'agile.systemField.estimatedStartTime',
    defaultShow: false,
    fieldType: 'datetime',
  },
  {
    code: 'estimatedEndTime',
    name: '预计结束时间',
    defaultShow: false,
    nameKey: 'agile.systemField.estimatedEndTime',
    fieldType: 'datetime',
  },
  {
    code: 'actualStartTime',
    name: '实际开始时间',
    defaultShow: false,
    nameKey: 'agile.systemField.actualStartTime',
    fieldType: 'datetime',
  },
  {
    code: 'actualEndTime',
    name: '实际结束时间',
    defaultShow: false,
    nameKey: 'agile.systemField.actualEndTime',
    fieldType: 'datetime',
  },
  {
    code: 'mainResponsibleIds',
    name: '主要负责人',
    defaultShow: false,
    nameKey: 'agile.systemField.mainResponsible',
    fieldType: 'member',
  }, {
    code: 'environment',
    name: '环境',
    nameKey: 'agile.systemField.environment',
    defaultShow: false,
    fieldType: 'multiple',
  },
  {
    code: 'creatorIds',
    name: '创建人',
    nameKey: 'agile.systemField.creator',
    defaultShow: false,
    fieldType: 'member',
  }, {
    code: 'updatorIds',
    name: '更新人',
    nameKey: 'agile.systemField.updater',
    defaultShow: false,
    fieldType: 'member',
  },
  {
    code: 'participantIds',
    name: '参与人',
    nameKey: 'agile.systemField.participant',
    defaultShow: false,
    fieldType: 'multiMember',
  },
  {
    code: 'storyPointsNull',
    name: '故事点为空',
    nameKey: 'agile.systemField.storyPointsNull',
    archive: true,
    defaultShow: false,
  },
  {
    code: 'remainingTimeNull',
    name: '剩余预估时间为空',
    nameKey: 'agile.systemField.remainingTimeNull',
    archive: true,
    defaultShow: false,
  },
  ];
  if (has('agile:PublishVersion')) {
    systemFields.push({
      code: 'tags',
      name: 'Tag',
      nameKey: 'agile.systemField.tag',
      defaultShow: false,
      fieldType: 'multiple',
    });
  }
  systemFields.push({
    code: 'storyPoints',
    name: '故事点',
    nameKey: 'agile.systemField.storyPoint',
    defaultShow: false,
    fieldType: 'number',
  });
  systemFields.push({
    code: 'remainingTime',
    name: '剩余预估时间',
    nameKey: 'agile.systemField.remainingTime',
    defaultShow: false,
    fieldType: 'number',
  });
  systemFields.push({
    code: 'estimateTime',
    name: '原始预估时间',
    nameKey: 'agile.systemField.estimateTime',
    defaultShow: false,
    fieldType: 'number',
  });
  return isInProgram() ? systemFields.filter((f) => !includes(excludeCodes, f.code)) : systemFields.filter((f) => f.code !== 'feature' && !includes(excludeCodes, f.code));
}
/**
 * 故事地图的筛选  不可进行保存筛选 里面有个归档字段
 * @param {*} excludeCodes
 * @returns
 */
export function getSystemFieldsInStoryMap(excludeCodes = []) {
  const fields = getSystemFields([...excludeCodes, 'influenceVersion', 'fixVersion']);
  const archiveField = { ...omit(fields.find((field) => field.code === 'version'), 'archive'), defaultShow: true };

  fields.push(archiveField);
  const sortFieldValue = {
    sprint: 10, version: 15, statusId: 20, assigneeId: 25, priorityId: 30, feature: 35, epic: 36,
  };
  return sortBy(fields.filter((i) => !i.archive), (field) => sortFieldValue[field.code] || 120);
}
class IssueStore {
  // 当前加载状态
  @observable loading = false;

  // 创建工作项窗口是否展开
  @observable createFlag = false;

  // 工作项详情是否展开
  @observable expand = false;

  // 当前选中 Issue 详细信息
  @observable selectedIssue = {};

  // 筛选列表是否显示
  @observable filterListVisible = false;

  @computed get getFilterListVisible() {
    return this.filterListVisible;
  }

  @action setFilterListVisible = (data) => {
    this.filterListVisible = data;
  }

  @observable updateFilterName = '';

  @computed get getUpdateFilterName() {
    return this.updateFilterName;
  }

  @action setUpdateFilterName(data) {
    this.updateFilterName = data;
  }

  // 控制导出模态框是否显示
  @observable exportModalVisible = false;

  @computed get getExportModalVisible() {
    return this.exportModalVisible;
  }

  @action setExportModalVisible(visible) {
    this.exportModalVisible = visible;
  }

  // 我的筛选列表
  @observable myFilters = [];

  @computed get getMyFilters() {
    return toJS(this.myFilters);
  }

  @action setMyFilters(data) {
    this.myFilters = data;
  }

  @observable projectInfo = {};

  @computed get getProjectInfo() {
    return toJS(this.projectInfo);
  }

  @action setProjectInfo(data) {
    this.projectInfo = data;
  }

  @observable selectedMyFilterInfo = {};

  @computed get getSelectedMyFilterInfo() {
    return this.selectedMyFilterInfo;
  }

  @observable editFilterInfo = [];

  @computed get getEditFilterInfo() {
    return toJS(this.editFilterInfo);
  }

  @action setEditFilterInfo(data) {
    this.editFilterInfo = data;
  }

  @action setLoading(data) {
    this.loading = data;
  }

  @computed get getLoading() {
    return toJS(this.loading);
  }

  @action createQuestion(data) {
    this.createFlag = data;
  }

  @computed get getCreateQuestion() {
    return this.createFlag;
  }

  @action setClickedRow(data) {
    this.selectedIssue = data.selectedIssue;
    this.expand = data.expand;
  }

  @computed get getSelectedIssue() {
    return toJS(this.selectedIssue);
  }

  @action setSelectedIssue(data) {
    this.selectedIssue = data;
  }

  @computed get getExpand() {
    return toJS(this.expand);
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

  @action setDefaultAssignee = (id, data) => {
    this.defaultAssignee = data;
  }
}

export default new IssueStore();
