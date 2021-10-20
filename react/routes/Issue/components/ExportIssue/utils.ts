import { findIndex, flatMap, isEmpty } from 'lodash';
import { FieldProps } from 'choerodon-ui/pro/lib/data-set/Field';
import { stores } from '@choerodon/boot';
import { IExportSearch } from '@/api';

const { AppState } = stores;
function transformSystemFilter(data: any): Omit<IExportSearch, 'exportFieldCodes'> {
  const userId = String(AppState.userInfo.id);
  const {
    issueTypeId,
    assigneeId,
    statusId,
    priorityId,
    issueIds,
    quickFilterIds = [],
    createDate = [],
    updateDate = [],
    estimatedStartTime = [],
    estimatedEndTime = [],
    actualStartTime = [],
    actualEndTime = [],
    contents,
    component,
    epic,
    feature,
    label,
    reporterIds,
    sprint,
    summary,
    fixVersion,
    influenceVersion,
    testResponsibleIds,
    mainResponsibleIds,
    environment,
    creatorIds,
    updatorIds,
    tags,
    participantIds,
  } = data;
  const starBeaconIndex = findIndex(quickFilterIds, (item) => item === 'myStarBeacon');
  let starBeacon;
  if (starBeaconIndex !== -1 && quickFilterIds.splice(starBeaconIndex, 1)) {
    starBeacon = true;
  }
  const myAssignedIndex = findIndex(quickFilterIds, (item) => item === 'myAssigned');
  let myAssigned;
  if (myAssignedIndex !== -1 && quickFilterIds.splice(myAssignedIndex, 1)) {
    myAssigned = true;
  }
  return {
    advancedSearchArgs: {
      issueTypeId,
      reporterIds,
      statusId,
      priorityId,
    },
    otherArgs: {
      assigneeId,
      issueIds,
      component,
      epic,
      feature,
      label,
      sprint,
      summary,
      fixVersion,
      influenceVersion,
      starBeacon,
      myAssigned,
      userId: starBeacon || myAssigned ? userId : undefined,
      testResponsibleIds,
      mainResponsibleIds,
      environment,
      creatorIds,
      updatorIds,
      tags,
      participantIds,
    },
    searchArgs: {
      estimatedStartTimeScopeStart: estimatedStartTime[0],
      estimatedStartTimeScopeEnd: estimatedStartTime[1],
      estimatedEndTimeScopeStart: estimatedEndTime[0],
      estimatedEndTimeScopeEnd: estimatedEndTime[1],
      actualStartTimeScopeStart: actualStartTime[0],
      actualStartTimeScopeEnd: actualStartTime[1],
      actualEndTimeScopeStart: actualEndTime[0],
      actualEndTimeScopeEnd: actualEndTime[1],
      createStartDate: createDate[0],
      createEndDate: createDate[1],
      updateStartDate: updateDate[0],
      updateEndDate: updateDate[1],

    },
    quickFilterIds,
    contents: !isEmpty(contents) ? flatMap([contents]) : undefined,
  };
}

const getExportFieldCodes = (data: Array<any>): string[] => {
  const fieldTransform = {
    issueNum: 'issueNum',
    issueId: 'summary',
    //  "description":
    issueTypeId: 'typeName',
    //  "projectName":
    assigneeId: 'assigneeName',
    // "assigneeRealName":
    reporterId: 'reporterName',
    //  "reporterRealName":
    //   "resolution":
    statusId: 'statusName',
    issueSprintVOS: 'sprintName',
    // "creationDate":
    lastUpdateDate: 'lastUpdateDate',
    priorityId: 'priorityName',
    //  "subTask":
    //  "remainingTime":
    fixVersion: 'fixVersionName',
    influenceVersion: 'influenceVersionName',
    epic: 'epicName',
    label: 'labelName',
    storyPoints: 'storyPoints',
    component: 'componentName',
    createUser: 'createdUserName',
    updateUser: 'lastUpdatedUserName',
    mainResponsibleUser: 'mainResponsibleName',
    environmentName: 'environmentName',
    tags: 'tags',
    participants: 'participant',
  };
  // @ts-ignore
  return data.map((code: string) => fieldTransform[code] || code);
};

const getReverseExportFieldCodes = (data: Array<any>) => {
  const fieldTransform = {
    issueNum: 'issueNum',
    summary: 'issueId',
    //  "description":
    typeName: 'issueTypeId',
    //  "projectName":
    assigneeName: 'assigneeId',
    // "assigneeRealName":
    reporterName: 'reporterId',
    //  "reporterRealName":
    //   "resolution":
    statusName: 'statusId',
    sprintName: 'issueSprintVOS',

    // "creationDate":
    lastUpdateDate: 'lastUpdateDate',

    priorityName: 'priorityId',
    //  "subTask":
    //  "remainingTime":
    fixVersionName: 'fixVersion',
    influenceVersionName: 'influenceVersion',
    mainResponsibleName: 'mainResponsibleUser',
    epicName: 'epic',
    labelName: 'label',
    storyPoints: 'storyPoints',
    componentName: 'component',
    createdUserName: 'createUser',
    lastUpdatedUserName: 'updateUser',
    tags: 'tags',
    participant: 'participants',
  };
  // @ts-ignore
  return data.map((code: string) => fieldTransform[code] || code);
};

function getFilterFormSystemFields(): FieldProps[] {
  return ([{
    name: 'statusId',
    label: '状态',
    valueField: 'id',
    textField: 'name',
  }, {
    name: 'sprint',
    label: '冲刺',
    valueField: 'sprintId',
    textField: 'sprintName',
  }, {
    name: 'issueTypeId',
    label: '工作项类型',
    valueField: 'id',
    textField: 'name',
  },
  {
    name: 'feature',
    label: '特性',
    valueField: 'issueId',
    textField: 'summary',
  },
  {
    name: 'epic',
    label: '所属史诗',
    valueField: 'issueId',
    textField: 'epicName',
  },
  {
    name: 'priorityId',
    label: '优先级',
    valueField: 'id',
    textField: 'name',
  }, {
    name: 'label',
    label: '标签',
    valueField: 'labelId',
    textField: 'labelName',
  }, {
    name: 'component',
    label: '模块',
    valueField: 'componentId',
    textField: 'name',
  }, {
    name: 'fixVersion',
    label: '修复的版本',
    valueField: 'versionId',
    textField: 'name',
  }, {
    name: 'influenceVersion',
    label: '影响的版本',
    valueField: 'versionId',
    textField: 'name',
  }]);
}
export {
  getExportFieldCodes, getReverseExportFieldCodes, getFilterFormSystemFields, transformSystemFilter as getTransformSystemFilter,
};
