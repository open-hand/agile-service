import React, { useMemo } from 'react';
import {
  Form, DataSet, TextField, TextArea,
} from 'choerodon-ui/pro';

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
    }],
  }), []);
  return (
    <Form style={{ width: 512 }} dataSet={dataSet}>
      <TextField name="title" />
      <TextArea name="description" />
    </Form>
  );
};
export default BaseInfo;
