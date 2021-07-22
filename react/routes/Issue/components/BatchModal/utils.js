import { find, pick } from 'lodash';

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
  }],
  ['epicId', {
    id: 'epicId',
    code: 'epicId',
    name: '所属史诗',
    fieldType: 'single',
  }],
  ['featureId', {
    id: 'featureId',
    code: 'featureId',
    name: '所属特性',
    fieldType: 'single',
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
    format: (value, labelIssueRelVOList) => labelIssueRelVOList,
  }],
  ['componentIssueRelVOList', {
    id: 'componentIssueRelVOList',
    code: 'componentIssueRelVOList',
    name: '模块',
    fieldType: 'multiple',
    format: (value, component) => component,
  }],
  ['influenceVersion', {
    id: 'influenceVersion',
    code: 'influenceVersion',
    name: '影响的版本',
    fieldType: 'multiple',
    format: (value, influenceVersion) => pick(influenceVersion, ['versionId', 'name']),
  }],
  ['fixVersion', {
    id: 'fixVersion',
    code: 'fixVersion',
    name: '修复的版本',
    fieldType: 'multiple',
    format: (value, fixVersion) => pick(fixVersion, ['versionId', 'name']),
  }],
  ['storyPoints', {
    id: 'storyPoints',
    code: 'storyPoints',
    name: '故事点',
    fieldType: 'number',
  }],
  ['remainingTime', {
    id: 'remainingTime',
    code: 'remainingTime',
    name: '预估时间',
    fieldType: 'number',
  }],
  ['estimatedStartTime', {
    id: 'estimatedStartTime',
    code: 'estimatedStartTime',
    name: '预计开始时间',
    fieldType: 'datetime',
  }],
  ['estimatedEndTime', {
    id: 'estimatedEndTime',
    code: 'estimatedEndTime',
    name: '预计结束时间',
    fieldType: 'datetime',
  }],
  ['mainResponsibleId', {
    id: 'mainResponsibleId',
    code: 'mainResponsibleId',
    name: '主要负责人',
    fieldType: 'member',
  }],
  ['environment', {
    id: 'environment',
    code: 'environment',
    name: '环境',
    fieldType: 'single',
  }],
  ['tags', {
    id: 'tags',
    code: 'tags',
    name: 'Tag',
    fieldType: 'multiple',
  }],
]);

function transformValue(dataSet, key, value, format) {
  if (!value || !format) {
    return value;
  }
  function transform(v) {
    const lookup = dataSet.getField(key).getLookupData(v);
    return format(v, lookup);
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
