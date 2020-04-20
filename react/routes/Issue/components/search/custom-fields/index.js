import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import { debounce } from 'lodash';
import { toJS } from 'mobx';
import IssueStore from '@/stores/project/sprint/IssueStore';
import IssueTypeField from './field/IssueTypeField';
import StatusField from './field/StatusField';
import AssignField from './field/AssignField';
import ReporterField from './field/ReporterField';
import SprintField from './field/SprintField';
import ComponentField from './field/ComponentField';
import VersionField from './field/VersionField';
import CreateDateField from './field/CreateDateField';
// 自定义字段
import SelectField from './field/SelectField';
import InputField from './field/InputField';
import NumberField from './field/NumberField';
import MemberField from './field/MemberField';
import TimeField from './field/TimeField';
import Store from '../../../stores';


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
          <AssignField
            field={field} 
            value={value}
            onChange={handleChange}
          />
        );
      case 'reporterIds':
        return (
          <ReporterField
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
      case 'version':
        return (
          <VersionField
            field={field} 
            value={value}
            onChange={handleChange}
          />
        );
      case 'createDate':
        return (
          <CreateDateField
            field={field} 
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
        <TimeField
          field={field} 
          value={value}
          onChange={handleChange}
        />
      );
    default: return null;
  }
}
function CustomFields() {
  const { chosenFields } = IssueStore;
  const {
    dataSet, 
  } = useContext(Store);
  return [...chosenFields.entries()].map(([, field]) => <div key={field.code} style={{ margin: '0 5px' }}>{renderField(field)}</div>);
}

export default observer(CustomFields);
