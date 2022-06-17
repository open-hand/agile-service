import React from 'react';
import {
  Select, TextField, DatePicker, TimePicker, DateTimePicker, NumberField, TextArea, UrlField, DataSet, CheckBox,
} from 'choerodon-ui/pro';
import { observable } from 'mobx';
import { includes } from 'lodash';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { DatePickerProps } from 'choerodon-ui/pro/lib/date-picker/DatePicker';
import moment from 'moment';
import {
  IField,
} from '@/common/types';
import SelectUser from '@/components/select/select-user';
import Editor from '@/components/Editor';

import SelectEnvironment from '@/components/select/select-environment';
import SelectFeature from '@/components/select/select-feature';
import SelectCustomField from '@/components/select/select-custom-field';
import SelectMultiServiceTag from '@/components/select/select-multi-service-tag';
import DateTimePickerWithFormat from '@/components/date-time-picker/date-time-pikcer-format';
import { FORMAT_FIELDS } from '@/constants/DATE_FORMAT';
import SelectComponent from '../select/select-component';
import SelectProduct from '../select/select-product';

const multipleCodes = ['label', 'component', 'fixVersion', 'influenceVersion'];
export default function renderField<T extends Partial<SelectProps>>({
  field, otherComponentProps, dataSet, isInProgram,
}: {field: IField, otherComponentProps: T | Partial<DatePickerProps> | any, dataSet: DataSet, isInProgram: boolean}) {
  const {
    fieldCode, fieldType, fieldName, fieldOptions, defaultValue, system, fieldId,
  } = field;
  if (system) {
    switch (fieldCode) {
      case 'sprint':
      case 'status':
      case 'label':
      case 'priority':
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
      case 'component': {
        return (
          <SelectComponent
            name={fieldCode}
            clearButton
            multiple={includes(multipleCodes, fieldCode)}
            {...otherComponentProps}
          />
        );
      }
      case 'environment': {
        return <SelectEnvironment name={fieldCode} clearButton placeholder={fieldName} {...otherComponentProps} />;
      }
      case 'description': {
        return <Editor name={fieldCode} placeholder={fieldName} colSpan={2} style={{ height: 280 }} />;
      }
      case 'tag': {
        return <SelectMultiServiceTag name={fieldCode} multiple clearButton {...otherComponentProps} defaultValue={defaultValue} />;
      }
      case 'product':
        return <SelectProduct name={fieldCode} multiple clearButton {...otherComponentProps} />;
      case FORMAT_FIELDS[0]:
      case FORMAT_FIELDS[1]:
      case FORMAT_FIELDS[2]:
      case FORMAT_FIELDS[3]: {
        return (
          <DateTimePickerWithFormat
            name={fieldCode}
            style={{ width: '100%' }}
            defaultPickerValue={fieldCode === 'actualEndTime' || fieldCode === 'estimatedEndTime' ? moment().endOf('d') : undefined}
            {...otherComponentProps}
          />
        );
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
          maxLength={fieldCode === 'epicName' ? 44 : 100}
          valueChangeAction={'input' as any}
          style={{ width: '100%' }}
          {...otherComponentProps}
        />
      );
    case 'text':
      return (
        <TextArea
          name={fieldCode}
          autoSize
          maxLength={255}
          valueChangeAction={'input' as any}
          style={{ width: '100%' }}
          // @ts-ignore
          {...otherComponentProps}
        />
      );
    // case 'url':
    //   return (
    //     <UrlField
    //       name={fieldCode}
    //       {...otherComponentProps}
    //     />
    //   );
    case 'single': case 'multiple': case 'radio': case 'checkbox':
      return (
        <SelectCustomField
          name={fieldCode}
          style={{ width: '100%' }}
          multiple={fieldType === 'multiple' || fieldType === 'checkbox'}
          fieldId={fieldId as string}
          selected={defaultValue}
          {...otherComponentProps}
        />
      );
    case 'multiMember':
    case 'member':
    {
      return (
        <SelectUser
          multiple={fieldType === 'multiMember'}
          // eslint-disable-next-line no-nested-ternary
          selected={defaultValue ? (typeof defaultValue === 'string' ? [defaultValue] : defaultValue.map((item: any) => String(item))) : undefined}
          style={{ width: '100%' }}
          name={fieldCode}
          {...otherComponentProps}
        />
      );
    }
    default:
    {
      return (
        <TextField
          name={fieldCode}
          maxLength={100}
          valueChangeAction={'input' as any}
          style={{ width: '100%' }}
          {...otherComponentProps}
        />
      );
    }
  }
}
