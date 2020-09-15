import React, { useEffect } from 'react';
import {
  Form, Select, DataSet, Modal,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { IModalProps, IReportContentType } from '@/common/types';
import AddChart from '../add-chart';
import AddText from '../add-text';
import AddIssueList from '../add-issue-list';

interface Props {
  modal?: IModalProps,
  type: IReportContentType
}
const Components = new Map<IReportContentType, React.ComponentType>([
  ['chart', AddChart],
  ['text', AddText],
  ['list', AddIssueList],
  ['static_list', AddIssueList],
]);
const AddModal: React.FC<Props> = ({
  modal, type,
}) => {
  async function handleSubmit() {
    // if (dataSet.validate()) {
    //   const id = dataSet.current?.toData().field;
    //   const addFiledData = store.allFieldData.get(id);
    //   if (addFiledData) {
    //     onSubmitLocal(store.allFieldData.get(id), true);
    //   } else {
    //     const deleteRecord = store.getDeleteRecords.find((record) => record.get('id') === id);
    //     deleteRecord && onRestoreLocal(deleteRecord);
    //   }
    //   dataSet.create();
    //   return true;
    // }
    return false;
  }
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, []);
  const Component = Components.get(type);
  if (Component) {
    return (
      <div><Component /></div>
    );
  }
  return null;
};

const openAddModal = (props: Props) => {
  Modal.open({
    key: Modal.key(),
    title: '添加',
    style: {
      width: props.type === 'text' ? 380 : 1088,
    },
    drawer: true,
    children: <AddModal {...props} />,
  });
};
export default openAddModal;
