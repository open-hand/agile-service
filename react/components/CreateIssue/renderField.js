import React from 'react';
import {
  Input, InputNumber,
  TimePicker, DatePicker,
} from 'choerodon-ui';
import moment from 'moment';
import SelectUser from '@/components/select/select-user-old';
import { MAX_NUMBER_VALUE, MAX_FLOAT_BITE } from '@/constants/MAX_VALUE';
import SelectRadioOld from '@/components/select/select-radio-old';
import SelectCheckBoxOld from '@/components/select/select-checkbox-old';
import SelectFocusLoad from '../SelectFocusLoad';

const { TextArea } = Input;
export default function renderField(field, projectId) {
  const {
    fieldType, required, fieldName,
  } = field;
  if (fieldType === 'radio') {
    return (
      <SelectRadioOld
        label={fieldName}
        fieldId={field.fieldId}
        projectId={projectId}
      />
    );
  } if (field.fieldType === 'checkbox') {
    return (
      <SelectCheckBoxOld
        label={fieldName}
        fieldId={field.fieldId}
        projectId={projectId}
      />
    );
  } if (field.fieldType === 'time') {
    return (
      <TimePicker
        label={fieldName}
        placeholder={fieldName}
        style={{ display: 'block', width: '100%' }}
        defaultOpenValue={moment('00:00:00', 'HH:mm:ss')}
        allowEmpty={!required}
      />
    );
  } if (field.fieldType === 'datetime') {
    return (
      <DatePicker
        showTime
        label={fieldName}
        placeholder={fieldName}
        format="YYYY-MM-DD HH:mm:ss"
        style={{ display: 'block' }}
        allowClear={!required}
      />
    );
  } if (field.fieldType === 'date') {
    return (
      <DatePicker
        label={fieldName}
        placeholder={fieldName}
        format="YYYY-MM-DD"
        style={{ display: 'block' }}
        allowClear={!required}
      />
    );
  } if (field.fieldType === 'single') {
    return (
      <SelectFocusLoad
        type="custom_field"
        label={fieldName}
        allowClear={!required}
        getPopupContainer={(triggerNode) => document.getElementsByClassName('c7n-modal-body')[0]}
        requestArgs={{
          fieldId: field.fieldId,
          selected: field.defaultValue,
          projectId,
        }}
      />
    );
  } if (field.fieldType === 'multiple') {
    return (
      <SelectFocusLoad
        type="custom_field"
        label={fieldName}
        allowClear={!required}
        getPopupContainer={(triggerNode) => document.getElementsByClassName('c7n-modal-body')[0]}
        requestArgs={{
          fieldId: field.fieldId,
          selected: field.defaultValue,
          projectId,
        }}
        mode="multiple"
      />
    );
  } if (field.fieldType === 'number') {
    return (
      <InputNumber
        style={{ display: 'block', width: '100%' }}
        label={fieldName}
        step={field.extraConfig ? 0.01 : 1}
        precision={field.extraConfig ? MAX_FLOAT_BITE : 0}
        max={MAX_NUMBER_VALUE}
      />
    );
  } if (field.fieldType === 'text') {
    return (
      <TextArea
        style={{ display: 'block', width: '100%' }}
        autosize
        label={fieldName}
        maxLength={255}
      />
    );
  } if (['member', 'multiMember'].includes(field.fieldType)) {
    return (
      <SelectUser
        label={fieldName}
        allowClear
        mode={field.fieldType === 'multiMember' ? 'multiple' : undefined}
        projectId={projectId}
        extraOption={field.fieldType === 'multiMember' ? field.defaultValueObjs : field.defaultValueObj}
        className="multiMemberSelect"
      />
    );
  }
  return (
    <Input
      label={fieldName}
      maxLength={100}
    />
  );
}
