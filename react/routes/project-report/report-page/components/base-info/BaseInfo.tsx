import React, { useMemo, useImperativeHandle, useCallback } from 'react';
import {
  Form, DataSet, TextField, TextArea,
} from 'choerodon-ui/pro';
import { toJS } from 'mobx';
import { observer } from 'mobx-react-lite';
import { Prompt } from 'react-router-dom';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import SelectUser from '@/components/select/select-user';
import { useProjectReportContext } from '../../context';
import useFormatMessage from '@/hooks/useFormatMessage';

const BaseInfo: React.FC = () => {
  const {
    store, baseInfoRef, edit,
  } = useProjectReportContext();
  const formatMessage = useFormatMessage();
  const initData = useMemo(() => {
    if (edit) {
      const { ccList = [], receiverList = [] } = store.baseInfo || {};
      return [{
        ...store.baseInfo,
        ccList: ccList?.slice(),
        receiverList: receiverList?.slice(),
      }];
    }
    return undefined;
  }, [edit, store.baseInfo]);
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    data: initData,
    fields: [{
      name: 'title',
      label: '报告主题',
      maxLength: 44,
      required: true,
    }, {
      name: 'description',
      label: '报告说明',
      maxLength: 255,
    }, {
      name: 'receiverList',
      type: 'object' as FieldType,
      label: formatMessage({ id: 'agile.projectReport.receiver' }),
      required: true,
      textField: 'realName',
      valueField: 'id',
      multiple: true,
    }, {
      name: 'ccList',
      type: 'object' as FieldType,
      label: '抄送人',
      textField: 'realName',
      valueField: 'id',
      multiple: true,
    }],
    events: {
      update: () => {
        store.dirty = true;
      },
    },
  }), [initData, store]);
  const handleSubmit = useCallback(async () => {
    if (await dataSet.validate()) {
      const data = dataSet.current?.toData();
      return {
        title: data.title,
        description: data.description,
        receiverList: data.receiverList,
        ccList: data.ccList,
      };
    }
    return false;
  }, [dataSet]);
  useImperativeHandle(baseInfoRef, () => ({
    submit: handleSubmit,
  }));
  const selectedReceiverList = useMemo(() => (edit ? toJS(store.baseInfo?.receiverList) : undefined), [edit, store.baseInfo?.receiverList]);
  const selectedCCList = useMemo(() => (edit ? toJS(store.baseInfo?.ccList) : undefined), [edit, store.baseInfo?.ccList]);
  return (
    <>
      <Prompt message={edit ? '项目报告保存__@.@__项目报告有更改，放弃更改？' : '项目报告保存__@.@__项目报告未保存，放弃更改？'} when={store.dirty} />
      <Form style={{ width: 600, marginLeft: 18 }} dataSet={dataSet}>
        <TextField name="title" placeholder="请输入报告主题，例如：XXX冲刺项目汇报" />
        <TextArea
          name="description"
          // @ts-ignore
          resize="vertical"
          placeholder="请输入报告描述，报告的进一步说明，相当于报告的摘要信息"
        />
        <SelectUser name="receiverList" selectedUser={selectedReceiverList} />
        <SelectUser name="ccList" selectedUser={selectedCCList} clearButton />
      </Form>
    </>

  );
};
export default observer(BaseInfo);
