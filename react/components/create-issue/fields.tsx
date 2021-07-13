import React from 'react';
import {
  TextField, SelectBox,
  TextArea, NumberField, DatePicker, DateTimePicker, TimePicker,
} from 'choerodon-ui/pro';

import SelectUser from '@/components/select/select-user';
import { IFieldType, ISystemFieldCodeMap } from '@/common/types';
import UserTag from '../tag/user-tag';
import SelectIssueType from '../select/select-issue-type';
// import DateTimePicker from '../date-time-picker';
import Editor from '../CKEditor';
import SelectCustomField from '../select/select-custom-field';
import SelectSprint from '../select/select-sprint';
import SelectComponent from '../select/select-component';
import SelectLabel from '../select/select-label';
import SelectEpic from '../select/select-epic';
import SelectFeature from '../select/select-feature';
import SelectVersion from '../select/select-version';
import SelectNumber from '../select/select-number';
import SelectPriority from '../select/select-priority';
import SelectStatus from '../select/select-status';
import SelectMultiServiceTag from '../select/select-multi-service-tag';

type ProRenderFieldPropsType = {
  render: ((
    text: any,
    props: any,
    dom: JSX.Element,
  ) => JSX.Element)
  renderFormItem: ((props: any) => JSX.Element)
  /** 创建时提交至服务端的值和field的code的对应 */
  valueKey?: string
};
const valueTypeMap: Record<IFieldType | 'default', ProRenderFieldPropsType> = {
  default: {
    render: (text) => text,
    renderFormItem: (props) => (
      <TextField {...props} />
    ),
  },
  input: {
    render: (text) => text,
    renderFormItem: (props) => (
      <TextField {...props} />
    ),
  },
  text: {
    render: (text) => text,
    renderFormItem: (props) => (
      <TextArea {...props} />
    ),
  },
  member: {
    render: (text) => <UserTag data={text} />,
    renderFormItem: (props) => (
      <SelectUser {...props} />
    ),
  },
  multiMember: {
    render: (text) => <UserTag data={text} />,
    renderFormItem: (props) => (
      <SelectUser {...props} />
    ),
  },
  single: {
    render: (text) => <UserTag data={text} />,
    renderFormItem: (props) => (
      <SelectCustomField {...props} />
    ),
  },
  multiple: {
    render: (text) => <UserTag data={text} />,
    renderFormItem: (props) => (
      <SelectCustomField {...props} />
    ),
  },
  radio: {
    render: (text) => <UserTag data={text} />,
    renderFormItem: (props) => (
      <SelectBox {...props} />
    ),
  },
  checkbox: {
    render: (text) => <UserTag data={text} />,
    renderFormItem: (props) => (
      <SelectBox {...props} />
    ),
  },
  number: {
    render: (text) => <UserTag data={text} />,
    renderFormItem: (props) => (
      <NumberField {...props} />
    ),
  },
  time: {
    render: (text) => text,
    renderFormItem: (props) => (
      <TimePicker {...props} />
    ),
  },
  date: {
    render: (text) => text,
    renderFormItem: (props) => (
      <DatePicker {...props} />
    ),
  },
  datetime: {
    render: (text) => text,
    renderFormItem: (props) => (
      <DateTimePicker {...props} />
    ),
  },

};
const systemFieldMap: Record<ISystemFieldCodeMap, ProRenderFieldPropsType> = {
  summary: {
    render: (text) => text,
    renderFormItem: (props) => (
      <TextField {...props} />
    ),
  },
  issueType: {
    render: (text) => <UserTag data={text} />,
    renderFormItem: (props) => (
      <SelectIssueType {...props} />
    ),
    valueKey: 'issueTypeId',
  },
  description: {
    render: (text) => text,
    renderFormItem: (props) => (
      <Editor {...props} />
    ),
  },
  sprint: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectSprint {...props} />
    ),
    valueKey: 'sprintId',
  },
  estimatedStartTime: {
    render: (text) => text,
    renderFormItem: (props) => (
      <DateTimePicker {...props} />
    ),
  },
  estimatedEndTime: {
    render: (text) => text,
    renderFormItem: (props) => (
      <DateTimePicker {...props} />
    ),
  },
  component: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectComponent {...props} />
    ),
  },
  label: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectLabel {...props} />
    ),
  },
  epic: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectEpic {...props} />
    ),
  },
  assignee: {
    ...valueTypeMap.member,
    valueKey: 'assigneeId',
  },
  reporter: {
    ...valueTypeMap.member,
    valueKey: 'reporterId',
  },
  feature: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectFeature {...props} />
    ),
  },
  version: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectVersion {...props} />
    ),
  },
  fixVersion: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectVersion {...props} />
    ),
  },
  mainResponsible: valueTypeMap.member,
  storyPoints: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectNumber {...props} />
    ),
  },
  priority: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectPriority {...props} />
    ),
    valueKey: 'priorityId',
  },
  status: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectStatus {...props} />
    ),
  },
  remainingTime: valueTypeMap.datetime,
  tag: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectMultiServiceTag {...props} />
    ),
  },
};
const fieldMap = {
  ...valueTypeMap,
  ...systemFieldMap,
};
export { fieldMap };
// @ts-ignore
const getFieldConfig = (field: { fieldCode: string, fieldType: string }) => fieldMap[field.fieldCode] ?? fieldMap[field.fieldType] ?? fieldMap.default;
export default getFieldConfig;
