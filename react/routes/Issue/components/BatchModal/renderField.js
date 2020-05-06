import React, { Fragment } from 'react';

import {
  TextField, Select, DatePicker, TimePicker, DateTimePicker, CheckBox, NumberField, TextArea, UrlField,
} from 'choerodon-ui/pro';

import SelectUser from './select-user';

const { Option } = Select;
const singleList = ['radio', 'single'];


function renderDefaultField({ code }) {
  switch (code) {
    case 'component':
      return <Select multiple name={code} searchable searchMatcher="name" />;
    default: return null;
  }
}
export default function renderField({ code, fieldType, fieldOptions }) {
  if (['component'].includes(code)) {
    return renderDefaultField({ code });
  }
  switch (fieldType) {
    case 'time':
      return (
        <TimePicker
          name={code}
        />         
      );
    case 'datetime':
      return (
        <DateTimePicker
          name={code}
        />
      );
    case 'date':
      return (
        <DatePicker
          name={code}
        />
      );
    case 'number':
      return (
        <div>
          <NumberField
            name={code}
            // step={isCheck ? 0.1 : 1}
          />
        </div>
      );
    case 'input':
      return (
        <TextField
          name={code}
          maxLength={100}
        />
      );
    case 'text':
      return (
        <TextArea
          name={code}
          rows={3}
          maxLength={255}
        />
      );
    case 'url':
      return (
        <UrlField
          name={code}
        />
      );
    case 'radio': case 'single': case 'checkbox': case 'multiple':
      return (
        <Fragment>
          <Select
            name={code}
            style={{ width: '100%', marginBottom: '20px' }}
            multiple={!(singleList.indexOf(fieldType) !== -1)}
          >
            {fieldOptions
              && fieldOptions.length > 0
              && fieldOptions.map((item) => {
                if (item.enabled) {
                  return (
                    <Option
                      value={item.tempKey || item.id}
                      key={item.tempKey || item.id}
                    >
                      {item.value}
                    </Option>
                  );
                }
                return [];
              })}
          </Select>          
        </Fragment>
      );
    case 'member':
      return (
        <SelectUser
          name={code}
        />
      );
    default:
      return null;
  }
}
