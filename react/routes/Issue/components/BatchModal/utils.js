/* eslint-disable camelcase */

import { find, pick } from 'lodash';
import EMPTY_VALUE from '@/constants/EMPTY_VALUE';

export const systemFields = new Map([
  ['statusId', {
    id: 'statusId',
    code: 'statusId',
    name: '状态',
    fieldType: 'single',
  }],
  ['assigneeId', {
    id: 'assigneeId',
    code: 'assigneeId',
    name: '经办人',
    fieldType: 'member',
    emptyValue: EMPTY_VALUE.value_0_string,
  }],
  ['reporterId', {
    id: 'reporterId',
    code: 'reporterId',
    name: '报告人',
    fieldType: 'member',
  }],
  ['sprintId', {
    id: 'sprintId',
    code: 'sprintId',
    name: '冲刺',
    fieldType: 'single',
    emptyValue: EMPTY_VALUE.value_0_string,
  }],
  ['epicId', {
    id: 'epicId',
    code: 'epicId',
    name: '所属史诗',
    fieldType: 'single',
    emptyValue: EMPTY_VALUE.value_0_string,
  }],
  ['featureId', {
    id: 'featureId',
    code: 'featureId',
    name: '所属特性',
    fieldType: 'single',
    emptyValue: EMPTY_VALUE.value_0_string,
  }],
  ['priorityId', {
    id: 'priorityId',
    code: 'priorityId',
    name: '优先级',
    fieldType: 'single',
  }],
  ['labelIssueRelVOList', {
    id: 'labelIssueRelVOList',
    code: 'labelIssueRelVOList',
    name: '标签',
    fieldType: 'multiple',
    emptyValue: EMPTY_VALUE.value_arr,
  }],
  ['componentIssueRelVOList', {
    id: 'componentIssueRelVOList',
    code: 'componentIssueRelVOList',
    name: '模块',
    fieldType: 'multiple',
    emptyValue: EMPTY_VALUE.value_arr,
  }],
  ['influenceVersion', {
    id: 'influenceVersion',
    code: 'influenceVersion',
    name: '影响的版本',
    fieldType: 'multiple',
    format: (value) => pick(value, ['versionId', 'name']),
    emptyValue: EMPTY_VALUE.value_arr,
  }],
  ['fixVersion', {
    id: 'fixVersion',
    code: 'fixVersion',
    name: '修复的版本',
    fieldType: 'multiple',
    format: (value) => pick(value, ['versionId', 'name']),
    emptyValue: EMPTY_VALUE.value_arr,
  }],
  ['storyPoints', {
    id: 'storyPoints',
    code: 'storyPoints',
    name: '故事点',
    fieldType: 'number',
    emptyValue: EMPTY_VALUE.value_null,
  }],
  ['remainingTime', {
    id: 'remainingTime',
    code: 'remainingTime',
    name: '预估时间',
    fieldType: 'number',
    emptyValue: EMPTY_VALUE.value_null,
  }],
  ['estimatedStartTime', {
    id: 'estimatedStartTime',
    code: 'estimatedStartTime',
    name: '预计开始时间',
    fieldType: 'datetime',
    emptyValue: EMPTY_VALUE.value_null,
  }],
  ['estimatedEndTime', {
    id: 'estimatedEndTime',
    code: 'estimatedEndTime',
    name: '预计结束时间',
    fieldType: 'datetime',
    emptyValue: EMPTY_VALUE.value_null,
  }],
  ['actualStartTime', {
    id: 'actualStartTime',
    code: 'actualStartTime',
    name: '实际开始时间',
    fieldType: 'datetime',
    emptyValue: EMPTY_VALUE.value_null,
  }],
  ['actualEndTime', {
    id: 'actualEndTime',
    code: 'actualEndTime',
    name: '实际结束时间',
    fieldType: 'datetime',
    emptyValue: EMPTY_VALUE.value_null,
  }],
  ['mainResponsibleId', {
    id: 'mainResponsibleId',
    code: 'mainResponsibleId',
    name: '主要负责人',
    fieldType: 'member',
    emptyValue: EMPTY_VALUE.value_0_string,
  }],
  ['environment', {
    id: 'environment',
    code: 'environment',
    name: '环境',
    fieldType: 'single',
    emptyValue: EMPTY_VALUE.value_null,
  }],
  ['tags', {
    id: 'tags',
    code: 'tags',
    name: 'Tag',
    fieldType: 'multiple',
  }],
  ['participantIds', {
    id: 'participantIds',
    code: 'participantIds',
    name: '参与人',
    fieldType: 'multiMember',
    emptyValue: EMPTY_VALUE.value_arr,
  }],
]);

function transformValue(dataSet, key, value, format) {
  if (!value && ('emptyValue' in systemFields.get(key))) {
    return systemFields.get(key)?.emptyValue;
  }
  if (!value || !format) {
    return value;
  }
  function transform(v) {
    return format(v);
  }
  if (Array.isArray(value)) {
    return value.map((v) => transform(v));
  }
  return transform(value);
}

export function formatFields(fieldData, data, dataSet) {
  const temp = {
    predefinedFields: {},
    customFields: [],
  };
  for (const key of Object.keys(data)) {
    if (systemFields.get(key)) {
      temp.predefinedFields[key] = transformValue(dataSet, key, data[key], systemFields.get(key).format);
    } else {
      const customField = find(fieldData, { code: key });
      temp.customFields.push({
        fieldId: customField.id,
        fieldType: customField.fieldType,
        value: data[key],
      });
    }
  }
  return temp;
}
