import { find } from 'lodash';

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
  dataRef: React.MutableRefObject<Map<string, any>>
  targetProjectId: string
}

const transformValue = ({
  k, v, dataRef, targetProjectId,
}: Props) => {
  const fieldCode = k.split('-')[1];
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
        const target = find(dataRef.current?.get('component') || [], { name: component });
        if (target) {
          componentList.push(target);
        } else {
          componentList.push({
            name: component && component.slice(0, 100),
            projectId: targetProjectId,
          });
        }
      });
      return componentList;
    }
    case 'label': {
      const labelList: any = [];
      (v || []).forEach((label: string) => {
        const target = find(dataRef.current?.get('label'), { labelName: label });
        if (target) {
          labelList.push(target);
        } else {
          labelList.push({
            labelName: label,
            projectId: targetProjectId,
          });
        }
      });
      return labelList;
    }
    case 'fixVersion': {
      const versionList: any[] = [];
      (v || []).forEach((versionId: string) => {
        const target = find(dataRef.current?.get('fixVersion'), { versionId });
        if (target) {
          versionList.push(target);
        }
      });
      return versionList;
    }
    default:
      break;
  }
};

export default transformValue;
