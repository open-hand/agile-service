import React, { useMemo, useCallback, useImperativeHandle } from 'react';
import {
  Form, DataSet, TextField,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import Editor from '@/components/Editor';
import { RefProps } from '../add-modal';
import { IReportTextBlock } from '../../store';

interface Props {
  innerRef: React.MutableRefObject<RefProps>
}
const AddText: React.FC<Props> = ({ innerRef }) => {
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'title',
      label: '文本标题',
      maxLength: 44,
      required: true,
    }, {
      name: 'description',
      required: true,
    }],
  }), []);
  const handleSubmit = useCallback(async () => {
    if (dataSet.validate()) {
      const data = dataSet.current?.toData();
      console.log(data);
      const block: IReportTextBlock = {
        id: String(Math.random()),
        title: data.title,
        type: 'text',
        content: JSON.stringify(data.description),
      };
      return block;
    }
    return false;
  }, [dataSet]);
  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  return (
    <Form dataSet={dataSet}>
      <TextField name="title" />
      <Editor name="description" placeholder="输入文本" />
    </Form>
  );
};
export default observer(AddText);
