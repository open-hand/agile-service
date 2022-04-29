import { IField } from '@/common/types';
import { FieldWithValue } from '../../store';

const transformValue = ({ issue, field, fieldsWithValue }: {issue: any, field: IField, fieldsWithValue: FieldWithValue[]}) => {
  const {
    fieldCode, system, fieldType, projectId,
  } = field;
  switch (fieldCode) {
    case 'status':
      return issue.statusVO?.name || '无';
    case 'component':
      return issue.componentIssueRelVOList && issue.componentIssueRelVOList.length ? (issue.componentIssueRelVOList).map((item: any) => item.name).join('、') : '无';
    case 'label':
      return issue.labelIssueRelVOList && issue.labelIssueRelVOList.length ? issue.labelIssueRelVOList.map((item: any) => item.labelName).join('、') : '无';
    case 'epic':
      return issue.issueEpicName || '无';
    case 'fixVersion':
      return issue.versionIssueRelVOList && issue.versionIssueRelVOList.length ? issue.versionIssueRelVOList.map((item: any) => item.name).join('、') : '无';
    case 'sprint':
      return (issue.activeSprint || (issue.closeSprint && issue.closeSprint.length)) ? [...(issue.activeSprint ? [issue.activeSprint] : []), ...(issue.closeSprint || [])].map((item: any) => item.sprintName).join('、') : '无';
    case 'assignee':
      return issue.assigneeRealName || '无';
    case 'pi':
      return (issue.activePi || (issue.closePi && issue.closePi.length)) ? [...(issue.activePi ? [issue.activePi] : []), ...(issue.closePi || [])].map((item: any) => `${item.code}-${item.name}`).join('、') : '无';
    case 'subProject':
      return issue.activePiTeams && issue.activePiTeams.length ? issue.activePiTeams.map((item: any) => item.name).join('、') : '无';
    case 'programVersion':
      return issue.programVersionFeatureRelVOS && issue.programVersionFeatureRelVOS.length ? issue.programVersionFeatureRelVOS.map((item: any) => item.name).join('、') : '无';
    case 'feature':
      return issue.featureName || '无';
    case 'reporter':
      return issue.reporterRealName || '无';
    case 'mainResponsible':
      return issue.mainResponsible?.realName || '无';
    case 'epicName':
      return issue.epicName || '无';
    case 'estimateTime':
      return issue.estimateTime !== null && issue.estimateTime !== undefined ? `${issue.estimateTime} 小时` : '无';
    default:
      break;
  }
  if (!system && fieldType === 'member' && !projectId) {
    const fieldItem = fieldsWithValue.find((item: FieldWithValue) => item.fieldCode === fieldCode);
    if (fieldItem) {
      return fieldItem.valueStr?.realName || '无';
    }
  }
  return '';
};

export default transformValue;
