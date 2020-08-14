import React, { useState, useEffect } from 'react';
import {
  Form, Select, DataSet, Modal, Axios,
} from 'choerodon-ui/pro/lib';
import { observer } from 'mobx-react-lite';
import { IModalProps } from '@/common/types';
import { pageConfigApi, PageConfigIssueType } from '@/api';
import issueTable from '@/routes/Issue/components/issue-table';
import { usePageIssueTypeStore } from '../../stores';
import PageIssueTypeStore from '../../stores/PageIssueTypeStore';

const { Option } = Select;
interface Props {
  modal?: IModalProps,
  dataSet: DataSet,
  store: PageIssueTypeStore,
}
interface IPage {
  name: string,
  fieldTypeName: string,
  id: string,
}
const AddFiled: React.FC<Props> = observer(({ modal, dataSet, store }) => {
  const [pageList, setPageList] = useState([] as IPage[]);
  async function handleSubmit() {
    if (dataSet.validate()) {
      return true;
    }
    return false;
  }
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, []);
  useEffect(() => {
    pageConfigApi.loadUnSelected(store.currentIssueType).then((res) => {
      const data = res.map((item) => store.allFieldData.get(item.id));
      setPageList(data);
    });
  }, []);
  return (
    <Form record={dataSet.current}>
      <Select name="field">
        {pageList.map((item) => <Option value={item.id}>{item.name}</Option>)}
      </Select>
    </Form>
  );
});

const openField = (dataSet: DataSet, store: PageIssueTypeStore) => {
  Modal.open({
    key: Modal.key(),
    title: '添加已有字段',
    style: {
      width: 340,
    },
    drawer: true,
    children: <AddFiled dataSet={dataSet} store={store} />,
  });
};
export default openField;
