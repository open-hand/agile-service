import React from 'react';
import {
  Select, TextField, DatePicker, TimePicker, DateTimePicker, NumberField, TextArea, UrlField, DataSet, CheckBox,
} from 'choerodon-ui/pro';
import { observable } from 'mobx';
import { includes } from 'lodash';
import {
  IField,
} from '@/common/types';
import SelectUser from '@/components/select/select-user';
import Editor from '@/components/Editor';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { DatePickerProps } from 'choerodon-ui/pro/lib/date-picker/DatePicker';

import SelectEnvironment from '@/components/select/select-environment';
import SelectFeature from '@/components/select/select-feature';

const { Option } = Select;
const forceUpdate = observable.box(false);
function noticeForceUpdate() {
  forceUpdate.set(true);
}

const multipleCodes = ['label', 'component', 'fixVersion', 'influenceVersion'];
export default function renderField<T extends Partial<SelectProps>>({
  field, otherComponentProps, dataSet, isInProgram,
}: {field: IField, otherComponentProps: T | Partial<DatePickerProps> | any, dataSet: DataSet, isInProgram: boolean}) {
  const {
    fieldCode, fieldType, fieldName, fieldOptions, defaultValue, system,
  } = field;
  if (system) {
    switch (fieldCode) {
      case 'sprint':
      case 'status':
      case 'priority':
      case 'label':
      case 'component':
      case 'fixVersion':
      case 'influenceVersion':
        return (
          <Select
            name={fieldCode}
            multiple={includes(multipleCodes, fieldCode)}
            {...otherComponentProps}
          />
        );
      case 'epic':
        // @ts-ignore
        return !isInProgram ? (
          <Select
            name={fieldCode}
            {...otherComponentProps}
          />
        ) : (
          <SelectFeature name="featureId" {...otherComponentProps} />
        );
      case 'environment': {
        return <SelectEnvironment name={fieldCode} clearButton placeholder={fieldName} {...otherComponentProps} />;
      }
      case 'description': {
        return <Editor name={fieldCode} placeholder={fieldName} colSpan={2} style={{ height: 280 }} />;
      }
      default:
        break;
    }
  }

  switch (fieldType) {
    case 'time': {
      return (
        <TimePicker
          name={fieldCode}
          style={{ width: '100%' }}
          {...otherComponentProps}

        />
      );
    }

    case 'datetime': {
      return (
        <DateTimePicker
          name={fieldCode}
          style={{ width: '100%' }}
          {...otherComponentProps}
        />
      );
    }
    case 'date':
      return (
        <DatePicker
          name={fieldCode}
          style={{ width: '100%' }}
          {...otherComponentProps}
        />
      );
    case 'number':
      return (
        <div>
          <NumberField
            name={fieldCode}
            style={{ width: '100%' }}
            // @ts-ignore
            {...otherComponentProps}
          // step={isCheck ? 0.1 : 1}
          />
        </div>
      );
    case 'input':
      return (
        <TextField
          name={fieldCode}
          maxLength={100}
          style={{ width: '100%' }}
          {...otherComponentProps}
        />
      );
    case 'text':
      return (
        <TextArea
          name={fieldCode}
          rows={3}
          maxLength={255}
          style={{ width: '100%' }}
          // @ts-ignore
          {...otherComponentProps}
        />
      );
    case 'url':
      return (
        <UrlField
          name={fieldCode}
          {...otherComponentProps}
        />
      );
    case 'radio': case 'single': case 'checkbox': case 'multiple':
      return (
        <Select
          name={fieldCode}
          style={{ width: '100%' }}
          multiple={fieldType === 'checkbox' || fieldType === 'multiple'}
          {...otherComponentProps}
        >
          {fieldOptions
            && fieldOptions.length > 0
            && fieldOptions.map((item) => (
              <Option
                value={item.id}
                key={item.id}
              >
                {item.value}
              </Option>
            ))}
        </Select>
      );
    case 'multiMember':
    case 'member':
    {
      return (
        <SelectUser
          multiple={fieldType === 'multiMember'}
          autoQueryConfig={defaultValue ? {
            selectedUserIds: (typeof defaultValue === 'string' ? [defaultValue] : defaultValue).map((item: any) => String(item)),
          } : undefined}
          style={{ width: '100%' }}
          name={fieldCode}
          {...otherComponentProps}
        />
      );
    }
    default:
      return (
        <TextField
          name={fieldCode}
          maxLength={100}
          style={{ width: '100%' }}
          {...otherComponentProps}
        />
      );
  }
}
