import { pageConfigApi, pageConfigApiConfig } from '@/api';
import { IModalProps } from '@/common/types';
import renderEditor from '@/routes/page-config/page-issue-type/components/sort-table/renderEditor';
import {
  Form, DataSet, Modal, Select,
} from 'choerodon-ui/pro/lib';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer } from 'mobx-react-lite';
import React, { useEffect } from 'react';

interface Props {
    record: Record
    ds: DataSet
    options: Array<{ name: string, code: string }>
    modal?:IModalProps
}
const EditSystemField: React.FC<Props> = observer(({
  record, ds, options, modal,
}) => {
  async function handleSubmit() {
    const syncIssueType = ds.current?.get('syncIssueType');
    const id = ds.current?.get('id');
    if (await ds.submit() !== false) {
      syncIssueType && Array.isArray(syncIssueType) && pageConfigApi.syncDefaultValue(id, String(syncIssueType));
      return true;
    }
    return false;
  }
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, []);
  return (
    <Form dataSet={ds}>
      {renderEditor({ record, name: 'defaultValue' })}
      <Select name="syncIssueType">
        {options.map((option) => <Select.Option value={option.code}>{option.name}</Select.Option>)}
      </Select>
    </Form>
  );
});
const openEditSystemField = (record: Record) => {
  const fieldCode = record.get('code');
  const data = { ...record.toData(), fieldCode };
  const ds = new DataSet({
    autoCreate: false,
    autoQuery: false,
    data: [data],
    fields: [{
      name: 'defaultValue', label: record.get('name'),

    }, {
      name: 'syncIssueType', label: '将默认值同步到', multiple: true,
    },
    ],
    transport: {
      submit: ({ data: submitData }) => pageConfigApiConfig.updateField(data.id, submitData[0]),
    },
  });
  const contextNameArr = String(data.contextName).split(',');
  Modal.open({
    key: Modal.key(),
    title: '修改默认值',
    drawer: true,
    children: <EditSystemField
      record={ds.current!}
      ds={ds}
      options={(data.contexts as Array<any>).map((item: any, index) => ({ code: item, name: contextNameArr[index] }))}
    />,
    style: {
      width: 380,
    },
    okText: '修改',
    cancelText: '取消',
  });
};
export default openEditSystemField;
