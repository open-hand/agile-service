import React from 'react';
import {
  TextField,
  TextArea, NumberField, DatePicker, DateTimePicker, TimePicker,
} from 'choerodon-ui/pro';

import moment from 'moment';
import SelectUser from '@/components/select/pro/select-user';
import { IFieldType, ISystemFieldCodeMap } from '@/common/types';
import UserTag from '../tag/user-tag';
import SelectIssueType from '../select/pro/select-issue-type';
import DateTimePickerWithDefault from '../date-time-picker';
import Editor from '../Editor';
import SelectCustomField from '../select/select-custom-field';
import SelectCustomFieldBox from '../select/select-custom-field-box';
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
import SelectParentIssue from '../select/select-parent-issue';
import SelectEnvironment from '../select/select-environment';
import SelectFeatureType from '../select/select-feature-type';
import SelectTeam from '../select/select-team';
import SelectUserWithAssigneeMe from '../select/pro/select-user-with-assigneeme';

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
      <TextArea
        // rows={1}
        autoSize
        {...props}
      />
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
      <SelectCustomFieldBox {...props} />
    ),
  },
  checkbox: {
    render: (text) => <UserTag data={text} />,
    renderFormItem: (props) => (
      <SelectCustomFieldBox {...props} />
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
      <SelectIssueType showIcon {...props} />
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
      <DateTimePickerWithDefault {...props} />
    ),
  },
  estimatedEndTime: {
    render: (text) => text,
    renderFormItem: (props) => (
      <DateTimePickerWithDefault defaultPickerValue={moment().endOf('d')} {...props} />
    ),
  },
  actualStartTime: {
    render: (text) => text,
    renderFormItem: (props) => (
      <DateTimePickerWithDefault {...props} />
    ),
  },
  actualEndTime: {
    render: (text) => text,
    renderFormItem: (props) => (
      <DateTimePickerWithDefault defaultPickerValue={moment().endOf('d')} {...props} />
    ),
  },
  component: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectComponent {...props} />
    ),
    valueKey: 'componentIssueRelVOList',
  },
  label: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectLabel combo {...props} />
    ),
    valueKey: 'labelIssueRelVOList',
  },
  epic: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectEpic {...props} />
    ),
    valueKey: 'epicId',
  },
  assignee: {
    ...valueTypeMap.member,
    renderFormItem: (props) => (
      <SelectUserWithAssigneeMe {...props} />
    ),
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
    valueKey: 'featureId',
  },
  influenceVersion: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectVersion valueField="versionId" {...props} />
    ),
  },
  fixVersion: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectVersion valueField="versionId" statusArr={['version_planning']} {...props} />
    ),
  },
  mainResponsible: {
    ...valueTypeMap.member,
    valueKey: 'mainResponsibleId',
  },
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
  remainingTime: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectNumber {...props} />
    ),
  },
  environment: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectEnvironment {...props} />
    ),
  },
  tag: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectMultiServiceTag {...props} />
    ),
  },
  parentIssueId: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectParentIssue {...props} />
    ),
  },
  // 项目群字段
  featureType: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectFeatureType {...props} />
    ),
  },
  subProject: {
    render: (text) => text,
    renderFormItem: (props) => (
      <SelectTeam {...props} />
    ),
    valueKey: 'teamProjectIds',
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
