import React, { useEffect, useRef } from 'react';

import {
  TextField, Select, DatePicker, TimePicker, DateTimePicker, NumberField, TextArea, UrlField,
} from 'choerodon-ui/pro';
import { toJS } from 'mobx';
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
import { userApi } from '@/api';
import SelectStatus from './field/StatusField';
import FeatureProjectField from './field/FeatureProjectField';

const { Option } = Select;
const singleList = ['radio', 'single'];

export default function renderField<T extends Partial<SelectProps>>(field: IChosenFieldField, selectOtherProps?: T) {
  const {
    code, fieldType, name, fieldOptions, value, id,
  } = field;
  const defaultValue = toJS(value);
  if (!id) {
    switch (code) {
      case 'sprint':
        // return <Select name={code} required />;
        return <SelectSprint name={code} statusList={[]} {...selectOtherProps} />;
      case 'statusId':
        return <SelectStatus name={code} multiple {...selectOtherProps} />;
      case 'issueTypeId':
        return <SelectIssueType name={code} multiple {...selectOtherProps} />;
      case 'epic':
        // @ts-ignore
        return <SelectEpic name={code} multiple {...selectOtherProps} />;
      case 'priorityId':
        // @ts-ignore
        return <SelectPriority name={code} multiple {...selectOtherProps} />;
      case 'label':
        // @ts-ignore
        return <SelectLabel name={code} multiple {...selectOtherProps} />;
      case 'component':
        // @ts-ignore
        return <SelectComponent name={code} multiple {...selectOtherProps} />;
      case 'version':
        // @ts-ignore
        return <SelectVersion name={code} {...selectOtherProps} />;
      case 'feature': {
        // @ts-ignore
        return <FeatureProjectField name={code} multiple featureIds={defaultValue} {...selectOtherProps} />;// label={name} style={{ width: '100%' }}
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
          multiple
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
    {
      return (
        <SelectUser
          label="user"
          multiple
          // @ts-ignore
          selectedUser={defaultValue.map((item: string) => ({ id: item }))}
          // request={(({ filter, page }) => userApi.getAllInProject(filter, page).then((res) => {
          //   if (res.list && Array.isArray(res.list)) {

          //   }
          // }))}
          style={{ width: '100%' }}
          name={code}
        />
      );
    }
    default:
      return null;
  }
}
