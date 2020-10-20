import React from 'react';
import {
  TextField, Select, DatePicker, TimePicker, DateTimePicker, NumberField, TextArea,
} from 'choerodon-ui/pro';
import { toJS } from 'mobx';
import moment, { Moment } from 'moment';
import SelectSprint from '@/components/select/select-sprint';
import SelectIssueType from '@/components/select/select-issue-type';
import SelectEpic from '@/components/select/select-epic';
import SelectPriority from '@/components/select/select-priority';
import SelectLabel from '@/components/select/select-label';
import SelectComponent from '@/components/select/select-component';
import SelectVersion from '@/components/select/select-version';
import SelectSubProject from '@/components/select/select-sub-project';
import SelectUser from '@/components/select/select-user';
import SelectStatus from './components/StatusField';
import FeatureProjectField from './components/FeatureProjectField';
import PIField from './components/pi-field';
import QuickFilterField from './components/quick-filter-field';
import InputField from './components/InputField';
import OldNumberField from './components/NumberField';
import { IFilterField, ICustomField } from '../filter/useFilter';

const { Option } = Select;
export function getFieldElement(field: IFilterField, isFilter?: boolean): React.ReactNode {
  const { code, fieldType, system } = field;
  if (system) {
    switch (code) {
      case 'sprint':
      case 'sprintList':
        return (
          <SelectSprint
            statusList={[]}
            isProgram={code === 'sprintList'}
            multiple
            hasUnassign
          />
        );
      case 'statusId':
      case 'statusList':
        return <SelectStatus isProgram={code === 'statusList'} multiple />;
      case 'issueTypeId':
      case 'issueTypeList':
        return <SelectIssueType isProgram={code === 'issueTypeList'} filterList={code === 'issueTypeList' ? [] : undefined} multiple />;
      case 'epic':
      case 'epicList':
        // @ts-ignore
        return <SelectEpic isProgram={code === 'epicList'} multiple />;
      case 'priorityId':
        // @ts-ignore
        return <SelectPriority multiple />;
      case 'label':
        // @ts-ignore
        return <SelectLabel multiple valueField="labelId" />;
      case 'component':
        // @ts-ignore
        return <SelectComponent valueField="componentId" multiple />;
      case 'version':
        // @ts-ignore
        return <SelectVersion valueField="versionId" />;
      case 'feature': {
        // @ts-ignore
        return <FeatureProjectField multiple />;
      }
      case 'teamProjectList': {
        return <SelectSubProject multiple />;
      }
      case 'piList': {
        return (
          <PIField
            multiple
          />
        );
      }
      case 'quickFilterIds': {
        return <QuickFilterField multiple />;
      }
      default:
        break;
    }
  }
  const { fieldOptions, value } = field as ICustomField;
  const defaultValue = toJS(value);
  switch (fieldType) {
    case 'time': {
      return (
        <TimePicker />
      );
    }

    case 'datetime':
      return (
        <DateTimePicker />
      );
    case 'date':
      return (
        <DatePicker />
      );
    case 'number':
      // @ts-ignore
      return isFilter ? <OldNumberField /> : (
        <NumberField />
      );
    case 'input':
      // @ts-ignore
      return isFilter ? <InputField /> : (
        <TextField
          maxLength={100}
        />
      );
    case 'text':
      return isFilter ? (
        // @ts-ignore
        <InputField />
      ) : (
        <TextArea
          rows={3}
          maxLength={255}
        />
      );
    case 'radio': case 'single': case 'checkbox': case 'multiple':
      return (
        <Select>
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
    case 'member':
    {
      return (
        <SelectUser
          multiple
            // @ts-ignore
          selectedUserIds={defaultValue ? defaultValue.map((item: string) => ({ id: item })) : undefined}
        />
      );
    }
    default: return null;
  }
}
function encodeDate(date: string, isTime: boolean): Moment
function encodeDate(date: string[], isTime: boolean): Moment[]
function encodeDate(date: string | string[], isTime: boolean) {
  if (Array.isArray(date)) {
    return date.map((d) => (d ? moment(isTime ? `2000-01-01 ${d}` : d) : d));
  }
  if (date) {
    return moment(isTime ? `2000-01-01 ${date}` : date);
  }
  return date;
}
function decodeDate(date: Moment, isTime: boolean): string
function decodeDate(date: Moment[], isTime: boolean): string[]
function decodeDate(date: Moment | Moment[], isTime: boolean) {
  if (Array.isArray(date)) {
    return date.map((d) => (d ? moment(d).format(isTime ? 'HH:mm:ss' : 'YYYY-MM-DD HH:mm:ss') : d));
  }
  if (date) {
    return moment(date).format(isTime ? 'HH:mm:ss' : 'YYYY-MM-DD HH:mm:ss');
  }
  return date;
}
export { encodeDate, decodeDate };
