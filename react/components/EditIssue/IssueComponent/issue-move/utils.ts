import { find, includes } from 'lodash';
import { IField, Issue } from '@/common/types';
import { MoveTargetIssue } from './store';

export function initTargetIssue(sourceIssue: Issue): MoveTargetIssue {
  const { summary } = sourceIssue;
  // @ts-ignore
  const targetIssue = {
    summary,
    customFields: new Map(),
  } as MoveTargetIssue;
  return targetIssue;
}

const filterFields = (arr: IField[], typeCode: string) => {
  const excludeCodes: string[] = ['summary', 'issueType', 'description', 'remainingTime', 'storyPoints', 'priority', 'estimatedStartTime', 'estimatedEndTime', 'benfitHypothesis', 'acceptanceCritera', 'environment', 'tag', 'participant', 'product'];
  if (typeCode === 'feature') {
    excludeCodes.push('sprint');
  }
  return arr.filter((item) => (item.system || (!item.system && !item.projectId && item.fieldType === 'member')) && !includes(excludeCodes, item.fieldCode));
};

export const getFinalFields = ({ fields, typeCode, targetProjectType }: { fields: IField[], typeCode: string, targetProjectType: string }) => {
  const statusField = {
    fieldName: '状态',
    fieldCode: 'status',
    system: true,
    code: 'status',
    required: true,
  } as IField;

  const reporterField = {
    fieldName: '报告人',
    fieldCode: 'reporter',
    fieldType: 'member',
    system: true,
    code: 'reporter',
    required: true,
  } as IField;
  const resAdded = [
    ...(fields || []),
    reporterField,
  ];
  // 后端返回了就不加了
  if (!find(fields, { fieldCode: 'status' })) {
    resAdded.unshift(statusField);
  }
  const filtered = filterFields(resAdded, typeCode);
  if (targetProjectType === 'subProject' && typeCode === 'story') {
    const epicFieldIndex = filtered.findIndex((item) => item.fieldCode === 'epic');
    if (epicFieldIndex > -1) {
      const featureField = {
        fieldName: '特性',
        fieldCode: 'feature',
        system: true,
        code: 'feature',
      } as IField;
      filtered.splice(epicFieldIndex, 1, featureField);
    }
  }
  return filtered;
};

export function split(str: string, gutter: string): string[] {
  for (let i = str.length - 1; i >= 0; i -= 1) {
    if (str[i] === gutter) {
      return [str.slice(0, i), str.slice(i + 1)];
    }
  }
  return [str];
}
