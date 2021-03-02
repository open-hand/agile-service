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
  data?:IReportTextBlock
}
const AddText: React.FC<Props> = ({ innerRef, data: editData }) => {
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    data: editData ? [{ title: editData.title, description: JSON.parse(editData.content) }] : undefined,
    fields: [{
      name: 'title',
      label: '文本标题',
      maxLength: 44,
      required: true,
    }, {
      name: 'description',
      required: true,
    }],
  }), [editData]);
  const handleSubmit = useCallback(async () => {
    if (await dataSet.validate()) {
      const data = dataSet.current?.toData();
      const text = data.description;
      const block: IReportTextBlock = {
        key: String(Math.random()),
        title: data.title,
        type: 'text',
        content: text,
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
