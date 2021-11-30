import { DataSet } from 'choerodon-ui/pro';
import { castArray, find, pick } from 'lodash';
import { IField } from '@/common/types';

export const systemFields = new Map([
  ['description', {
    id: 'description',
  }],
  ['status', {
    id: 'statusId',
  }],
  ['assignee', {
    id: 'assigneeId',
  }],
  ['reporter', {
    id: 'reporterId',
  }],
  ['sprint', {
    id: 'sprintId',
  }],
  ['epic', {
    id: 'epicId',
  }],
  ['featureId', {
    id: 'featureId',
  }],
  ['priority', {
    id: 'priorityId',
  }],
  ['label', {
    id: 'labelIssueRelVOList',
    // @ts-ignore
    format: (value, labelIssueRelVOList) => labelIssueRelVOList,
  }],
  ['component', {
    id: 'componentIssueRelVOList',
    // @ts-ignore
    format: (value, component) => component,
  }],
  ['influenceVersion', {
    id: 'influenceVersion',
    // @ts-ignore
    format: (value, influenceVersion) => pick(influenceVersion, ['versionId', 'name']),
  }],
  ['fixVersion', {
    id: 'fixVersion',
    // @ts-ignore
    format: (value, fixVersion) => pick(fixVersion, ['versionId', 'name']),
  }],
  ['storyPoints', {
    id: 'storyPoints',
  }],
  ['remainingTime', {
    id: 'remainingTime',
  }],
  ['estimatedStartTime', {
    id: 'estimatedStartTime',
  }],
  ['estimatedEndTime', {
    id: 'estimatedEndTime',
  }],
  ['mainResponsible', {
    id: 'mainResponsibleId',
  }],
  ['environment', {
    id: 'environment',
  }],
  ['tag', {
    id: 'tags',
  }],
  ['participant', {
    id: 'participantIds',
  }],
]);

export function transformValue(dataSet: DataSet, key: string, value: any, format: (v: any, lookup: any) => any) {
  if (!value || !format) {
    return value;
  }
  function transform(v: any) {
    let lookup = dataSet.getField(key)?.getLookupData(v);

    if (key === 'component') {
      const options = dataSet.getState('component-options')?.toData() as any[];
      lookup = typeof v === 'object' ? v : options?.find((r) => r.componentId === v) || v;
    }

    return format(v, lookup);
  }
  if (Array.isArray(value)) {
    return value.map((v) => transform(v));
  }
  return transform(value);
}

export function formatFields(fieldData: IField[], data: object, dataSet: DataSet) {
  const temp: {
    predefinedFields: object
    customFields: {
      fieldId: string,
      fieldType: string,
      value: any,
    }[]
  } = {
    predefinedFields: {},
    customFields: [],
  };
  for (const key of Object.keys(data)) {
    const field = fieldData.find((item: IField) => item.fieldCode === key);
    if (systemFields.get(key) || field?.system) {
      // @ts-ignore
      temp.predefinedFields[systemFields.get(key)?.id || field?.fieldCode] = transformValue(dataSet, key, data[key], systemFields.get(key)?.format);
    } else {
      const customField = find(fieldData, { fieldCode: key });
      if (customField) {
        temp.customFields.push({
          fieldId: customField.fieldId,
          fieldType: customField.fieldType,
          // @ts-ignore
          value: data[key],
        });
      }
    }
  }
  return temp;
}

export const extraFields = ['timeTrace'];
