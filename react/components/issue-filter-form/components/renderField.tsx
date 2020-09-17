import React from 'react';

import {
  TextField, Select, DatePicker, TimePicker, DateTimePicker, NumberField, TextArea, UrlField,
} from 'choerodon-ui/pro';
import SelectUser from '@/components/select/select-user';
import SelectFeature from '@/components/select/select-feature';
import SelectSprint from '@/components/select/select-sprint';
import SelectIssueType from '@/components/select/select-issue-type';
import SelectEpic from '@/components/select/select-epic';
import SelectPriority from '@/components/select/select-priority';
import SelectLabel from '@/components/select/select-label';
import SelectComponent from '@/components/select/select-component';
import SelectVersion from '@/components/select/select-version';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IChosenFieldField } from '@/components/chose-field/types';
import SelectStatus from './field/StatusField';

const { Option } = Select;
const singleList = ['radio', 'single'];

export default function renderField<T extends Partial<SelectProps>>(field: IChosenFieldField, selectOtherProps?: T) {
  const {
    code, fieldType, name, fieldOptions, value: defaultValue, id,
  } = field;
  if (!id) {
    switch (code) {
      case 'sprint':
        return <Select name={code} required />;
        return <SelectSprint name={code} statusList={[]} {...selectOtherProps} />;
      case 'statusId':
        return <SelectStatus name={code} {...selectOtherProps} />;
      case 'issueTypeId':
        return <SelectIssueType name={code} multiple {...selectOtherProps} />;
      case 'epicId':
        // @ts-ignore
        return <SelectEpic name={code} {...selectOtherProps} />;
      case 'priorityId':
        // @ts-ignore
        return <SelectPriority name={code} {...selectOtherProps} />;
      case 'labelIssueRelVOList':
        // @ts-ignore
        return <SelectLabel name={code} {...selectOtherProps} />;
      case 'componentIssueRelVOList':
        // @ts-ignore
        return <SelectComponent name={code} {...selectOtherProps} />;
      case 'version':
        // @ts-ignore
        return <SelectVersion name={code} {...selectOtherProps} />;
      case 'featureId': {
        // @ts-ignore
        return <SelectFeature name={code} {...selectOtherProps} />;// label={name} style={{ width: '100%' }}
      }
      default:
        break;
    }
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
              return (
                <Option
                  value={item.id}
                  key={item.id}
                >
                  {item.value}
                </Option>
              );
              return [];
            })}
        </Select>
      );
    case 'member':
      return (
        <SelectUser
          label="user"
          style={{ width: '100%' }}
          name={code}
        />
      );
    default:
      return null;
  }
}
