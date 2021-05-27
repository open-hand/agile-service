import React from 'react';

import {
  TextField, Select, DatePicker, TimePicker, DateTimePicker, NumberField, TextArea, UrlField,
} from 'choerodon-ui/pro';
import moment from 'moment';
import SelectUser from '@/components/select/select-user';
import SelectFeature from '@/components/select/select-feature';
import EndDateTimePicker from '@/components/date-time-picker';
import SelectEnvironment from '@/components/select/select-environment';
import SelectCustomField from '@/components/select/select-custom-field';

const singleList = ['radio', 'single'];

export default function renderField({
  code, fieldType, fieldOptions, id,
}) {
  switch (code) {
    case 'componentIssueRelVOList': {
      return <Select style={{ width: '100%' }} multiple name={code} searchable searchMatcher="name" />;
    }
    case 'featureId': {
      return <SelectFeature name={code} style={{ width: '100%' }} />;
    }
    case 'estimatedEndTime': {
      return (
        <EndDateTimePicker
          name={code}
          style={{ width: '100%' }}
          defaultPickerValue={moment().endOf('d')}
        />
      );
    }
    case 'environment': {
      return (
        <SelectEnvironment
          name={code}
          style={{ width: '100%' }}
        />
      );
    }
    default: break;
  }
  switch (fieldType) {
    case 'time':
      return (
        <TimePicker
          name={code}
          style={{ width: '100%' }}
        />
      );
    case 'datetime':
      return (
        <DateTimePicker
          name={code}
          style={{ width: '100%' }}
        />
      );
    case 'date':
      return (
        <DatePicker
          name={code}
          style={{ width: '100%' }}
        />
      );
    case 'number':
      return (
        <div>
          <NumberField
            name={code}
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
          valueChangeAction="input"
          style={{ width: '100%' }}
        />
      );
    case 'text':
      return (
        <TextArea
          name={code}
          rows={3}
          maxLength={255}
          valueChangeAction="input"
          style={{ width: '100%' }}
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
        <SelectCustomField
          name={code}
          fieldId={id}
          style={{ width: '100%' }}
          multiple={!(singleList.indexOf(fieldType) !== -1)}
          searchable
        />
      );
    case 'multiMember':
    case 'member':
      return (
        <SelectUser
          multiple={fieldType === 'multiMember'}
          style={{ width: '100%' }}
          name={code}
        />
      );
    default:
      return null;
  }
}
