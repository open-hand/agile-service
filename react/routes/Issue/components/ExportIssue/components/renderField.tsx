import React from 'react';

import {
  TextField, Select, DatePicker, TimePicker, DateTimePicker, NumberField, TextArea, UrlField,
} from 'choerodon-ui/pro';
import SelectUser from '@/components/select/select-user';
import SelectFeature from '@/components/select/select-feature';
import { IExportIssueChosenField } from '../types';

const { Option } = Select;
const singleList = ['radio', 'single'];

export default function renderField(field: IExportIssueChosenField) {
  const {
    code, fieldType, name, fieldOptions, value: defaultValue,
  } = field;
  switch (code) {
    case 'componentIssueRelVOList': {
      return <Select label={name} style={{ width: '100%' }} multiple name={code} searchable searchMatcher="name" />;
    }
    case 'featureId': {
      // @ts-ignore
      return <SelectFeature name={code} label={name} style={{ width: '100%' }} />;
    }
    default: break;
  }
  switch (fieldType) {
    case 'time':
      return (
        <TimePicker
          label={name}
          name={code}
          style={{ width: '100%' }}
        />
      );
    case 'datetime':
      return (
        <DateTimePicker
          name={code}
          label={name}
          style={{ width: '100%' }}
        />
      );
    case 'date':
      return (
        <DatePicker
          name={code}
          label={name}
          style={{ width: '100%' }}
        />
      );
    case 'number':
      return (
        <div>
          <NumberField
            name={code}
            label={name}
            style={{ width: '100%' }}
          // step={isCheck ? 0.1 : 1}
          />
        </div>
      );
    case 'input':
      return (
        <TextField
          name={code}
          maxLength={100}
          label={name}
          style={{ width: '100%' }}
        />
      );
    case 'text':
      return (
        <TextArea
          name={code}
          rows={3}
          maxLength={255}
          label={name}
          style={{ width: '100%' }}
        />
      );
    case 'url':
      return (
        <UrlField
          label={name}
          name={code}
        />
      );
    case 'radio': case 'single': case 'checkbox': case 'multiple':
      return (
        <Select
          name={code}
          label={name}
          style={{ width: '100%' }}
          multiple={!(singleList.indexOf(fieldType) !== -1)}
        >
          {fieldOptions
            && fieldOptions.length > 0
            && fieldOptions.map((item) => {
              if (item.enabled) {
                return (
                  <Option
                    value={item.id}
                    key={item.id}
                  >
                    {item.value}
                  </Option>
                );
              }
              return [];
            })}
        </Select>
      );
    case 'member':
      return (
        <SelectUser
          label={name}
          style={{ width: '100%' }}
          name={code}
        />
      );
    default:
      return null;
  }
}
