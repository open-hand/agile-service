import React from 'react';
import { observer } from 'mobx-react-lite';
import { toJS } from 'mobx';
import { Icon } from 'choerodon-ui';
import IssueStore from '@/stores/project/issue/IssueStore';
import IssueTypeField from './field/IssueTypeField';
import StatusField from './field/StatusField';
import SprintField from './field/SprintField';
import ComponentField from './field/ComponentField';
import LabelField from './field/LabelField';
import VersionField from './field/VersionField';
import EpicField from './field/EpicField';
import FeatureField from './field/FeatureField';
import PriorityField from './field/PriorityField';
// 自定义字段
import SelectField from './field/SelectField';
import InputField from './field/InputField';
import NumberField from './field/NumberField';
import MemberField from './field/MemberField';
import DateTimeField from './field/DateTimeField';
import ChooseField from '../choose-field';

function renderField(field) {
  const { fieldType } = field;
  const { chosenFields } = IssueStore;
  const value = chosenFields.get(field.code) ? toJS(chosenFields.get(field.code).value) : undefined;
  const handleChange = (v) => {
    IssueStore.handleFilterChange(field.code, v);
  };
  // 系统自带字段
  if (!field.id) {
    switch (field.code) {
      case 'issueTypeId':
        return (
          <IssueTypeField
            field={field}
            value={value}
            onChange={handleChange}
          />
        );
      case 'statusId':
        return (
          <StatusField
            field={field}
            value={value}
            onChange={handleChange}
          />
        );
      case 'assigneeId':
        return (
          <MemberField
            field={field}
            value={value}
            onChange={handleChange}
          />
        );
      case 'reporterIds':
        return (
          <MemberField
            field={field}
            value={value}
            onChange={handleChange}
          />
        );
      case 'sprint':
        return (
          <SprintField
            field={field}
            value={value}
            onChange={handleChange}
          />
        );
      case 'component':
        return (
          <ComponentField
            field={field}
            value={value}
            onChange={handleChange}
          />
        );
      case 'label':
        return (
          <LabelField
            field={field}
            value={value}
            onChange={handleChange}
          />
        );
      case 'priorityId':
        return (
          <PriorityField
            field={field}
            value={value}
            onChange={handleChange}
          />
        );
      case 'version':
        return (
          <VersionField
            field={field}
            value={value}
            onChange={handleChange}
          />
        );
      case 'epic':
        return (
          <EpicField
            field={field}
            value={value}
            onChange={handleChange}
          />
        );
      case 'feature':
        return (
          <FeatureField
            field={field}
            value={value}
            onChange={handleChange}
          />
        );
      case 'createDate':
        return (
          <DateTimeField
            field={{
              fieldType: 'datetime',
              name: '创建时间',
            }}
            value={value}
            onChange={handleChange}
          />
        );
      default: return null;
    }
  }
  switch (fieldType) {
    case 'single':
    case 'multiple':
    case 'radio':
    case 'checkbox':
      return (
        <SelectField
          field={field}
          value={value}
          onChange={handleChange}
        />
      );
    case 'input':
    case 'text':
      return (
        <InputField
          field={field}
          value={value}
          onChange={handleChange}
        />
      );
    case 'member':
      return (
        <MemberField
          field={field}
          value={value}
          onChange={handleChange}
        />
      );
    case 'number':
      return (
        <NumberField
          field={field}
          value={value}
          onChange={handleChange}
        />
      );
    case 'time':
    case 'datetime':
    case 'date':
      return (
        <DateTimeField
          field={field}
          value={value}
          onChange={handleChange}
        />
      );
    default: return null;
  }
}
function CustomFields({ children }) {
  const { chosenFields } = IssueStore;
  const selectTypes = [];
  const dateTypes = [];
  const inputTypes = [];
  for (const [, field] of chosenFields) {
    if (['single', 'multiple', 'radio', 'checkbox', 'member'].includes(field.fieldType)) {
      selectTypes.push(field);
    } else if (['time', 'datetime', 'date'].includes(field.fieldType)) {
      dateTypes.push(field);
    } else if (['input', 'text', 'number'].includes(field.fieldType)) {
      inputTypes.push(field);
    }
  }

  const render = f => f.map(field => !field.noDisplay && (
    <div className="field" key={field.code} style={{ margin: '4px 5px', display: 'flex', alignItems: 'center' }}>
      {renderField(field)}
      
      {!field.defaultShow && (
        <div
          className="test"
          style={{
            cursor: 'pointer',
            borderRadius: '50%', 
            width: 14,
            height: 14,
            lineHeight: '11px',
            background: 'rgba(0,0,0,0.16)',
            color: 'white',
            textAlign: 'center', 
            marginLeft: 5,
          }}
          onClick={() => {
            IssueStore.handleChosenFieldChange(false, field);
          }}
        > 
          <Icon
            type="close"     
            style={{ fontSize: '10px' }}       
          />
        </div>
      )}
    </div>
  ));
  const types = [selectTypes, inputTypes, dateTypes].filter(arr => arr.length > 0);
  const result = types.map(type => <div style={{ display: 'flex', flexWrap: 'wrap', marginBottom: 4 }}>{render(type)}</div>);
  result[0].props.children.unshift(children);
  result[result.length - 1].props.children.push(<ChooseField key="choose" />);
  return (
    <div>
      {result}
    </div>
  );
}

export default observer(CustomFields);
