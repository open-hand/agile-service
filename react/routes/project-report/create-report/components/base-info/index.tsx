import React, { useMemo } from 'react';
import {
  Form, DataSet, TextField, TextArea,
} from 'choerodon-ui/pro';
import SelectUser from '@/components/select/select-user';

interface Props {

}
const BaseInfo: React.FC<Props> = () => {
  const dataSet = useMemo(() => new DataSet({
    fields: [{
      name: 'title',
      label: '报告主题',
      required: true,
    }, {
      name: 'description',
      label: '报告说明',
      required: true,
    }, {
      name: 'receiver',
      label: '收件人',
      required: true,
    }, {
      name: 'person',
      label: '抄送人',
    }],
  }), []);
  return (
    <Form style={{ width: 512, marginLeft: 18 }} dataSet={dataSet}>
      <TextField name="title" />
      <TextArea name="description" />
      <SelectUser name="receiver" />
      <SelectUser name="person" />
    </Form>
  );
};
export default BaseInfo;
