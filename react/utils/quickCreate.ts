import {
  get, isEmpty, isNumber, pick,
} from 'lodash';
import { fieldApi } from '@/api';
import { SHOW_FEATURE_TYPE_CODES_ALL } from '@/constants/SHOW_FEATURE_TYPE_CODE';
import { getProjectId } from './common';

function hasAvailableValue(field: any, value: any) {
  if (field.fieldType === 'multiple' || field.fieldType === 'checkbox' || field.fieldType === 'multiMember') {
    return !!value?.length;
  }
  return !!value;
}
export async function checkCanQuickCreate(typeId: string, assigneeId?: string, projectId?: string, defaultValues?: Record<string, any>) {
  const param = {
    schemeCode: 'agile_issue',
    issueTypeId: typeId,
    pageCode: 'agile_issue_create',
  };
  const checkDefaultValueKeys = ['estimatedStartTime', 'estimatedEndTime', 'actualStartTime', 'actualEndTime'];
  const whiteList = ['summary', 'status', 'reporter', 'issueType', 'priority', 'epicName'];
  const fields = await fieldApi.getFields(param, projectId);
  let requiredButNullFields = fields.filter((field: any) => !whiteList.includes(field.fieldCode) && field.required && !field.defaultValue);
  // 过滤已经有填充值
  const values = defaultValues && pick(defaultValues, checkDefaultValueKeys);
  requiredButNullFields = values && Object.keys(values).length > 0 ? requiredButNullFields.filter((field: any) => !hasAvailableValue(field, get(values, field.fieldCode))) : requiredButNullFields;
  if (!requiredButNullFields.length || (requiredButNullFields.length === 1 && requiredButNullFields[0].fieldCode === 'assignee' && assigneeId)) {
    return true;
  }
  return false;
}
export interface IQuickCreateDefaultValueParams {
  issueTypeId: string
  typeCode: string
  priorityId?: string
  projectId?: string
  programId?: string
  epicId?: string | number
  piId?: string
  summary?: string
  parentIssueId?: string | number
  relateIssueId?: string | number
  sprintId?: string
  featureVO?: { featureType: string } /** ? */
  wsjfVO?: any
  epicName?: string
  description?: string
  issueLinkCreateVOList?: Array<any>
  labelIssueRelVOList?: Array<any>
  versionIssueRelVOList?: Array<{ versionId: string, relationType: 'fix' | 'influence' }>
  featureId?: string
  assigneeId?: string
  reporterId?: string
  estimatedStartTime?: string
  estimatedEndTime?: string
  [otherPropsName: string]: any
}
export function getQuickCreateDefaultObj(defaultValues?: IQuickCreateDefaultValueParams, fieldsMap: Map<string, any> = new Map()): any {
  if (!defaultValues) {
    return {};
  }
  const { versionIssueRelVOList = [] } = defaultValues;
  const defaultVersionList: IQuickCreateDefaultValueParams['versionIssueRelVOList'] = [];
  if (!isEmpty(fieldsMap.get('influenceVersion')?.defaultValue) && !versionIssueRelVOList.some((item = {} as any) => item.relationType === 'influence')) {
    fieldsMap.get('influenceVersion')?.defaultValue.forEach((item: any) => defaultVersionList.push({
      versionId: item,
      relationType: 'influence',
    }));
  }
  if (!isEmpty(fieldsMap.get('fixVersion')?.defaultValue) && !versionIssueRelVOList.some((item = {} as any) => item.relationType === 'fix')) {
    fieldsMap.get('fixVersion')?.defaultValue.forEach((item: any) => defaultVersionList.push({
      versionId: item,
      relationType: 'fix',
    }));
  }
  versionIssueRelVOList.push(...defaultVersionList);

  return {
    ...defaultValues,
    summary: defaultValues.summary?.trim(),
    priorityCode: `priority-${defaultValues.priorityId || 0}`,
    priorityId: defaultValues.priorityId || 0,
    projectId: defaultValues.projectId || getProjectId(),
    programId: defaultValues.projectId || getProjectId(),
    epicId: defaultValues.epicId || fieldsMap.get('epic')?.defaultValue || 0,
    parentIssueId: defaultValues.parentIssueId || 0,
    relateIssueId: defaultValues.relateIssueId || 0,
    sprintId: defaultValues.sprintId || fieldsMap.get('sprint')?.defaultValue || 0,
    epicName: defaultValues.typeCode === 'issue_epic' ? defaultValues.summary?.trim() : undefined,
    componentIssueRelVOList: fieldsMap.get('component')?.defaultValueObjs || [],
    description: defaultValues.description || '',
    issueLinkCreateVOList: [],
    labelIssueRelVOList: fieldsMap.get('label')?.defaultValueObjs || [],
    versionIssueRelVOList,
    // fixVersionIssueRel: fieldsMap.get('fixVersion')?.defaultValue || [],
    featureId: SHOW_FEATURE_TYPE_CODES_ALL.includes(defaultValues.typeCode) ? defaultValues.featureId : 0,
    assigneeId: defaultValues.assigneeId || fieldsMap.get('assignee')?.defaultValue,
    reporterId: defaultValues.reporterId || fieldsMap.get('reporter')?.defaultValue,
    estimatedEndTime: defaultValues.estimatedEndTime || fieldsMap.get('estimatedEndTime')?.defaultValue,
    estimatedStartTime: defaultValues.estimatedStartTime || fieldsMap.get('estimatedStartTime')?.defaultValue,
    actualEndTime: defaultValues.actualEndTime || fieldsMap.get('actualEndTime')?.defaultValue,
    actualStartTime: defaultValues.actualStartTime || fieldsMap.get('actualStartTime')?.defaultValue,
    storyPoints: fieldsMap.get('storyPoints')?.defaultValue,
    remainingTime: fieldsMap.get('remainingTime')?.defaultValue,
    mainResponsibleId: fieldsMap.get('mainResponsible')?.defaultValue,
    testResponsibleId: fieldsMap.get('testResponsible')?.defaultValue,
    productIds: fieldsMap.get('product')?.defaultValue,
  };
}
