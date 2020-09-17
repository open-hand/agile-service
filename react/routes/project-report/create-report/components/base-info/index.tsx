import React, { useMemo, useImperativeHandle, useCallback } from 'react';
import {
  Form, DataSet, TextField, TextArea,
} from 'choerodon-ui/pro';
import SelectUser from '@/components/select/select-user';
import { useProjectReportContext } from '../../context';

interface Props {

}
const BaseInfo: React.FC<Props> = () => {
  const { baseInfoRef } = useProjectReportContext();
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'title',
      label: '报告主题',
      maxLength: 44,
      required: true,
    }, {
      name: 'description',
      label: '报告说明',
    }, {
      name: 'receiverList',
      label: '收件人',
      required: true,
      textField: 'realName',
      valueField: 'id',
      multiple: true,
    }, {
      name: 'ccList',
      label: '抄送人',
      textField: 'realName',
      valueField: 'id',
      multiple: true,
    }],
  }), []);
  const handleSubmit = useCallback(async () => {
    if (await dataSet.validate()) {
      const data = dataSet.current?.toData();
      return {
        title: data.title,
        description: data.description,
        receiverList: data.receiverList.map((id: string) => ({ id })),
        ccList: data.ccList.map((id: string) => ({ id })),
      };
    }
    return false;
  }, [dataSet]);
  useImperativeHandle(baseInfoRef, () => ({
    submit: handleSubmit,
  }));
  return (
    <Form style={{ width: 512, marginLeft: 18 }} dataSet={dataSet}>
      <TextField name="title" />
      <TextArea name="description" />
      <SelectUser name="receiverList" />
      <SelectUser name="ccList" clearButton />
    </Form>
  );
};
export default BaseInfo;
