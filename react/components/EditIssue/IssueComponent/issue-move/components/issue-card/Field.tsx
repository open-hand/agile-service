import React, { useCallback } from 'react';
import { IField } from '@/common/types';
import { observer } from 'mobx-react-lite';
import FieldStatus from './fields/FieldStatus';
import FieldUser from './fields/FieldUser';
import FieldLabel from './fields/FieldLabel';
import FieldSprint from './fields/FieldSprint';
import FieldComponent from './fields/FieldComponent';
import FieldEpic from './fields/FieldEpic';
import FieldFeature from './fields/FieldFeature';
import FieldProgramVersion from './fields/FieldProgramVersion';
import FieldPI from './fields/FieldPI';
// import FieldSubProject from './fields/FieldSubProject';
import FieldVersion from './fields/FieldVersion';
import FieldEstimateTime from './fields/FieldEstimateTime';
import store, { FieldWithValue, MoveTarget } from '../../store';

export interface FieldCommonProps {
  target: MoveTarget
  field: IField
  onChange?: (value: any | undefined, valueStr: any) => void
  fieldWithValue: FieldWithValue | undefined
  disabledSprint: boolean
}
export interface FieldProps {
  field: IField
  target: MoveTarget
  disabledSprint: boolean
}
function getFieldComponent(field: IField) {
  const { fieldCode, fieldType } = field;
  switch (fieldCode) {
    case 'status': {
      return FieldStatus;
    }
    case 'label': {
      return FieldLabel;
    }
    case 'sprint': {
      return FieldSprint;
    }
    case 'component': {
      return FieldComponent;
    }
    case 'epic': {
      return FieldEpic;
    }
    case 'feature': {
      return FieldFeature;
    }
    case 'programVersion': {
      return FieldProgramVersion;
    }
    case 'pi': {
      return FieldPI;
    }
    // case 'subProject': {
    //   return FieldSubProject;
    // }
    case 'fixVersion': {
      return FieldVersion;
    }
    case 'estimateTime': {
      return FieldEstimateTime;
    }
    default: break;
  }
  if (fieldType === 'member') {
    return FieldUser;
  }
  return null;
}
const Field: React.FC<FieldProps> = ({ field, target, disabledSprint }) => {
  const handleSubmit = useCallback((value, valueStr) => {
    // 主issue冲刺变了，把所有子任务和子bug的冲刺也改掉
    if (field.fieldCode === 'sprint') {
      store.issueMapValues.forEach(({ target: issueTarget }) => store.updateFieldValue(value, valueStr, field.fieldCode as string, issueTarget));
    } else {
      store.updateFieldValue(value, valueStr, field.fieldCode as string, target);
    }
  }, [field, target]);
  const component = getFieldComponent(field);
  if (!component) {
    return null;
  }
  const fieldWithValue = target.issue.customFields.get(field.fieldCode as string);
  return React.createElement(component, {
    target, field, fieldWithValue, onChange: handleSubmit, disabledSprint,
  });
};
export default observer(Field);
