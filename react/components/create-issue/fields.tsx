import React from 'react';
import { TextField, SelectBox } from 'choerodon-ui/pro';
import SelectUser from '@/components/select/select-user';
import UserTag from '../tag/user-tag';
import SelectIssueType from '../select/select-issue-type';
import DateTimePicker from '../date-time-picker';
import Editor from '../CKEditor';

type ProRenderFieldPropsType = {
  render: ((
    text: any,
    props: any,
    dom: JSX.Element,
  ) => JSX.Element)
  renderFormItem: ((props: any) => JSX.Element)
};
const valueTypeMap: Record<string, ProRenderFieldPropsType> = {
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
  member: {
    render: (text) => <UserTag data={text} />,
    renderFormItem: (props) => (
      <SelectUser {...props} />
    ),
  },
  datetime: {
    render: (text) => text,
    renderFormItem: (props) => (
      <DateTimePicker {...props} />
    ),
  },
  issueType: {
    render: (text) => <UserTag data={text} />,
    renderFormItem: (props) => (
      <SelectIssueType {...props} />
    ),
  },
  description: {
    render: (text) => text,
    renderFormItem: (props) => (
      <Editor {...props} />
    ),
  },
};
export default valueTypeMap;
