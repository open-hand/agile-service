import { find } from 'lodash';
import { split } from './utils';

export const submitFieldMap = new Map([
  ['status', 'statusId'],
  ['component', 'componentIssueRelVOList'],
  ['label', 'labelIssueRelVOList'],
  ['epic', 'epicId'],
  ['fixVersion', 'versionIssueRelVOList'],
  ['sprint', 'sprintId'],
  ['assignee', 'assigneeId'],
  ['pi', 'piId'],
  ['subProject', 'teamProjectIds'],
  ['programVersion', 'programVersionIds'],
  ['feature', 'featureId'],
  ['reporter', 'reporterId'],
  ['mainResponseible', 'mainResponsibleId'],
]);

interface Props {
  k: string,
  v: any,
  dataMap: Map<string, any>
  targetProjectId: string
}

const transformValue = ({
  k, v, dataMap, targetProjectId,
}: Props) => {
  const fieldCode = split(k, '-')[1];
  switch (fieldCode) {
    case 'programVersion':
    case 'subProject':
    case 'reporter':
    case 'feature':
    case 'report':
    case 'pi':
    case 'assignee':
    case 'sprint':
    case 'epic':
    case 'status':
      return v;
    case 'component': {
      const componentList: any[] = [];
      (v || []).forEach((component: string) => {
        const target = find(dataMap?.get('component') || [], { name: component });
        if (target) {
          componentList.push(target);
        }
      });
      return componentList.length ? componentList : undefined;
    }
    case 'label': {
      const labelList: any = [];
      (v || []).forEach((label: string) => {
        const target = find(dataMap?.get('label'), { labelName: label });
        if (target) {
          labelList.push(target);
        } else {
          labelList.push({
            labelName: label,
            projectId: targetProjectId,
          });
        }
      });
      return labelList.length ? labelList : undefined;
    }
    case 'fixVersion': {
      const versionList: any[] = [];
      (v || []).forEach((versionId: string) => {
        const target = find(dataMap?.get('fixVersion'), { versionId });
        if (target) {
          versionList.push(target);
        }
      });
      return versionList.length ? versionList : undefined;
    }
    default:
      break;
  }
  return false;
};

export default transformValue;
