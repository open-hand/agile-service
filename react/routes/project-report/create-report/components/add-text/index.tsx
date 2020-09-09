import React, { useMemo } from 'react';
import {
  Form, DataSet, TextField,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import Editor from '@/components/Editor';

const AddText: React.FC = () => {
  const dataSet = useMemo(() => new DataSet({
    fields: [{
      name: 'title',
      label: '文本标题',
      required: true,
    }, {
      name: 'description',
      required: true,
    }],
  }), []);

  return (
    <Form dataSet={dataSet}>
      <TextField name="title" />
      <Editor name="description" placeholder="输入文本" />
    </Form>
  );
};
export default observer(AddText);
