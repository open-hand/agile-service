import { IField } from '@/common/types';

export interface IFieldWithValue extends IField {
  value: any,
  valueStr: any,
}

const transformValue = ({ issue, field, fieldsWithValue }: {issue: any, field: IField, fieldsWithValue: IFieldWithValue[]}) => {
  const {
    fieldCode, system, fieldType, projectId,
  } = field;
  switch (fieldCode) {
    case 'status':
      return issue.statusVO?.name;
    case 'component':
      return (issue.componentIssueRelVOList || []).map((item: any) => item.name).join('、');
    case 'label':
      return (issue.labelIssueRelVOList || []).map((item: any) => item.labelName).join('、');
    case 'epic':
      return issue.issueEpicName;
    case 'fixVersion':
      return (issue.versionIssueRelVOList || []).map((item: any) => item.name).join('、');
    case 'sprint':
      return [...(issue.activeSprint ? [issue.activeSprint] : []), ...(issue.closeSprint || [])].map((item: any) => item.sprintName).join('、');
    case 'assignee':
      return issue.assigneeRealName;
    case 'pi':
      return [...(issue.activePi ? [issue.activePi] : []), ...(issue.closePi || [])].map((item: any) => `${item.code}-${item.name}`).join('、');
    case 'subProject':
      return (issue.activePiTeams || []).map((item: any) => item.name).join('、');
    case 'programVersion':
      return (issue.programVersionFeatureRelVOS || []).map((item: any) => item.name).join('、');
    case 'feature':
      return issue.featureName;
    case 'reporter':
      return issue.reporterRealName;
    default:
      break;
  }
  if (!system && fieldType === 'member' && !projectId) {
    const fieldItem = fieldsWithValue.find((item: IFieldWithValue) => item.fieldCode === fieldCode);
    if (fieldItem) {
      return fieldItem.valueStr?.realName;
    }
  }
  return '';
};

export default transformValue;
