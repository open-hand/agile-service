import React, { useMemo } from 'react';
import {
  Form, Select, DataSet, Modal,
} from 'choerodon-ui/pro/lib';
import { observer } from 'mobx-react-lite';

const { Option } = Select;
const AddFiled: React.FC<> = observer(() => {
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    paging: false,
    fields: [
      { name: 'field', label: '字段' },
    ],
  }), []);
  return (
    <Form dataSet={dataSet}>
      <Select name="field">
        <Option value="1">1</Option>
        <Option value="2">2</Option>
        <Option value="3">3</Option>
        <Option value="4">4</Option>
      </Select>
    </Form>
  );
});

export default () => {
  Modal.open({
    key: Modal.key(),
    title: '添加已有字段',
    style: {
      width: 340,
    },
    drawer: true,
    children: <AddFiled />,
  });
};
