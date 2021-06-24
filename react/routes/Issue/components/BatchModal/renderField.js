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
// eslint-disable-next-line import/no-cycle
import SelectMultiServiceTag from '@/components/select/select-multi-service-tag';
import { systemFields } from './utils';

const singleList = ['radio', 'single'];

export default function renderField({
  code, fieldType, fieldOptions, id, name,
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
          label="预计结束时间"
        />
      );
    }
    case 'environment': {
      return (
        <SelectEnvironment
          name={code}
          style={{ width: '100%' }}
          label="环境"
        />
      );
    }
    case 'tags': {
      return <SelectMultiServiceTag name={code} style={{ width: '100%' }} label="Tag" />;
    }
    default: break;
  }
  switch (fieldType) {
    case 'time':
      return (
        <TimePicker
          name={code}
          style={{ width: '100%' }}
          label={name}
        />
      );
    case 'datetime':
      return (
        <DateTimePicker
          name={code}
          style={{ width: '100%' }}
          label={name}
        />
      );
    case 'date':
      return (
        <DatePicker
          name={code}
          style={{ width: '100%' }}
          label={name}
        />
      );
    case 'number':
      return (
        <div>
          <NumberField
            name={code}
            style={{ width: '100%' }}
            label={name}
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
          label={name}
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
          label={name}
        />
      );
    case 'url':
      return (
        <UrlField
          name={code}
          label={name}
        />
      );
    case 'radio': case 'single': case 'checkbox': case 'multiple':
      return (
        systemFields.has(code) ? (
          <Select
            name={code}
            fieldId={id}
            style={{ width: '100%' }}
            multiple={!(singleList.indexOf(fieldType) !== -1)}
            searchable
            label={name}
          />
        ) : (
          <SelectCustomField
            name={code}
            fieldId={id}
            style={{ width: '100%' }}
            multiple={!(singleList.indexOf(fieldType) !== -1)}
            searchable
            label={name}
          />
        )
      );
    case 'multiMember':
    case 'member':
      return (
        <SelectUser
          multiple={fieldType === 'multiMember'}
          style={{ width: '100%' }}
          name={code}
          label={name}
        />
      );
    default:
      return null;
  }
}
