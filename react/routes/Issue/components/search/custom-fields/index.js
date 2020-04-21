import React from 'react';
import { observer } from 'mobx-react-lite';
import { toJS } from 'mobx';
import IssueStore from '@/stores/project/issue/IssueStore';
import IssueTypeField from './field/IssueTypeField';
import StatusField from './field/StatusField';
import SprintField from './field/SprintField';
import ComponentField from './field/ComponentField';
import VersionField from './field/VersionField';
// 自定义字段
import SelectField from './field/SelectField';
import InputField from './field/InputField';
import NumberField from './field/NumberField';
import MemberField from './field/MemberField';
import DateTimeField from './field/DateTimeField';

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
function CustomFields() {
  const { chosenFields } = IssueStore;
  return [...chosenFields.entries()].map(([, field]) => !field.noDisplay && <div key={field.code} style={{ margin: '8px 5px 0' }}>{renderField(field)}</div>);
}

export default observer(CustomFields);
