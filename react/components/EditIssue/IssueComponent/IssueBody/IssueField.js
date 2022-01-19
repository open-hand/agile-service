import React, { useContext, Fragment } from 'react';
import { toJS } from 'mobx';
import { observer } from 'mobx-react-lite';
import { includes } from 'lodash';
import useIsInProgram from '@/hooks/useIsInProgram';
import { SHOW_FEATURE_TYPE_CODES } from '@/constants/SHOW_FEATURE_TYPE_CODE';
import {
  FieldAssignee, FieldVersion, FieldStatus, FieldSprint, FieldText,
  FieldReporter, FieldPriority, FieldLabel, FieldFixVersion, FieldPI,
  FieldEpic, FieldDateTime, FieldComponent, FieldTimeTrace, FieldStoryPoint,
  FieldSummary, FieldInput, FieldTeam, FieldProgramSprint,
} from './Field';
import EditIssueContext from '../../stores';
import FieldPro from './Field/FieldPro';
import FieldStartTime from './Field/FieldStartTime';
import FieldEndTime from './Field/FieldEndTime';
import FieldProgramVersion from './Field/FieldProgramVersion';
import FieldMember from './Field/FieldMember';
import FieldCreator from './Field/FieldCreator';
import FieldUpdater from './Field/FieldUpdater';
import FieldEnvironment from './Field/FieldEnvironment';
import FieldTag from './Field/FieldTag';
import FieldActualStartTime from './Field/FieldActualStartTime';
import FieldActualEndTime from './Field/FieldActualEndTime';
import FieldParticipant from './Field/FieldParticipant';
import FieldEstimateTime from './Field/FieldEstimateTime';

const hideFields = ['priority', 'component', 'label', 'fixVersion', 'sprint', 'timeTrace', 'assignee'];

const IssueField = observer((props) => {
  const {
    store, applyType, saveFieldVersionRef, saveFieldFixVersionRef, disabled, isProgramIssue, isAgileProgram,
  } = useContext(EditIssueContext);
  const { isShowFeature } = useIsInProgram({ projectId: store.projectId });
  const renderNormalField = (field) => (<FieldPro {...props} field={field} />);
  const getFieldComponent = (field) => {
    const issue = store.getIssue;
    const activePiTeams = issue.activePiTeams || [];
    const teamIds = activePiTeams.map((team) => team.id);

    const { typeCode } = issue;
    switch (field.fieldCode) {
      case 'assignee':
        return (<FieldAssignee {...props} />);
      case 'influenceVersion':
        return (<FieldVersion {...props} saveRef={saveFieldVersionRef} />);
      case 'status':
        return (<FieldStatus {...props} />);
      case 'sprint':
        if (typeCode !== 'sub_task') {
          return (<FieldSprint {...props} />);
        }
        return (<FieldSprint {...props} disabled />);

      case 'reporter':
        return (<FieldReporter {...props} isProgramIssue={isProgramIssue} />);
      case 'priority':
        return (<FieldPriority {...props} />);
      case 'label':
        return (<FieldLabel {...props} />);
      case 'fixVersion':
        return (<FieldFixVersion {...props} saveRef={saveFieldFixVersionRef} />);
      case 'epic': // 包含 feature 当有子项目时 只有特性
        // 子任务、史诗不显示史诗
        if (['issue_epic', 'sub_task'].indexOf(typeCode) === -1) {
          return (<FieldEpic {...props} isAgileProgram={isAgileProgram} />);
        }
        return '';
      case 'creationDate':
      case 'lastUpdateDate':
        return (<FieldDateTime {...props} field={field} />);
      case 'component':
        return (<FieldComponent {...props} />);
      case 'timeTrace':
        return (<FieldTimeTrace {...props} />);
      case 'pi':
        return (<FieldPI {...props} />);
      case 'benfitHypothesis':
      case 'acceptanceCritera':
        return (<FieldText {...props} field={field} feature />);
      case 'summary':
        return (<FieldSummary {...props} field={field} />);
      case 'epicName':
        return (<FieldInput {...props} field={field} />);
      case 'remainingTime':
      case 'storyPoints':
        return (<FieldStoryPoint {...props} field={field} />);
      case 'subProject':
        return ([
          <FieldTeam {...props} field={field} />,
          <FieldProgramSprint {...props} field={field} key={teamIds} />,
        ]);
      case 'estimatedStartTime':
        return <FieldStartTime {...props} field={field} />;
      case 'estimatedEndTime':
        return <FieldEndTime {...props} field={field} />;
      case 'actualStartTime':
        return <FieldActualStartTime {...props} field={field} />;
      case 'actualEndTime':
        return <FieldActualEndTime {...props} field={field} />;
      case 'programVersion':
        return <FieldProgramVersion {...props} field={field} />;
      case 'mainResponsible':
        return <FieldMember {...props} field={field} />;
      case 'environment':
        return <FieldEnvironment {...props} field={field} />;
      case 'created_user':
        return <FieldCreator {...props} field={field} />;
      case 'last_updated_user':
        return <FieldUpdater {...props} field={field} />;
      case 'tag':
        return <FieldTag {...props} field={field} />;
      case 'participant':
        return <FieldParticipant {...props} field={field} />;
      case 'estimateTime':
        return null;
      default:
        return renderNormalField(field);
    }
  };
  const issue = store.getIssue;
  const { issueId, typeCode } = issue;
  let fields = applyType === 'program' ? toJS(store.customFields).filter((item) => hideFields.indexOf(item.fieldCode) === -1) : toJS(store.customFields);
  // 系统字段单独控制是否显示
  if (typeCode === 'sub_task') {
    fields = fields.filter((field) => ['epic'].indexOf(field.fieldCode) === -1);
  } else if (typeCode === 'issue_epic') {
    fields = fields.filter((field) => field.fieldCode !== 'epic');
  } else if (typeCode === 'feature') {
    // fields.splice(4, 0, { fieldCode: 'teams', fieldName: '负责团队和冲刺' });
    // fields.splice(4, 0, { fieldCode: 'teamSprint', fieldName: '团队Sprint' });
    // fields.splice(4, 0, { fieldCode: 'programVersion', fieldName: '团队Sprint' });
  }
  if (!store.detailShow) {
    const isFeatureVisible = isShowFeature && SHOW_FEATURE_TYPE_CODES.includes(typeCode);
    fields = fields.slice(0, isFeatureVisible ? 9 : 10);
  }

  const ruleHiddenFields = store.getRuleHiddenFields();
  return (
    <div className="c7n-content-wrapper IssueField">
      {issueId ? fields.filter((field) => !includes(ruleHiddenFields, field.fieldCode)).map((field) => <Fragment key={field.id}>{getFieldComponent(field)}</Fragment>) : ''}
    </div>
  );
});

export default IssueField;
