import React, { useCallback, useState, useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { Modal, CheckBox } from 'choerodon-ui/pro';
import { IModalProps } from '@/common/types';
import { uiApi } from '@/api';
import EditIssueStore from '@/components/EditIssue/stores/EditIssueStore';
import styles from './DeleteUI.less';

export interface IUi {
  creationDate: string
  fileName: string
  id: string
  issueId: string
  projectId: string
  url: string
}

interface Props {
  ui: IUi
  store: EditIssueStore
  modal?: IModalProps
}
const DeleteUI: React.FC<Props> = ({ ui, store, modal }) => {
  const [deleteType, setDeleteType] = useState('deleteOnly');
  const handleChange = useCallback((value) => {
    if (value) {
      setDeleteType(value);
    } else {
      setDeleteType(deleteType === 'deleteWithReply' ? 'deleteOnly' : 'deleteWithReply');
    }
  }, [deleteType]);

  const handleDelete = useCallback(() => {
    if (deleteType === 'deleteWithReply') {
      return uiApi.deleteLink(ui.id).then(() => {
        store.getLinkedUI();
      });
    }
    return uiApi.deleteUI(ui.id).then(() => {
      store.getLinkedUI();
    });
  }, [deleteType, store, ui.id]);

  useEffect(() => {
    modal?.handleOk(handleDelete);
  }, [handleDelete, modal]);
  return (
    <div className={styles.deleteUI}>
      <p style={{
        marginBottom: 23,
      }}
      >
        {`您确定要删除“${ui.fileName}”吗？`}
      </p>
      <div>
        <CheckBox
          name="delete"
          value="deleteOnly"
          onChange={handleChange}
          checked={deleteType === 'deleteOnly'}
          style={{
            marginRight: 10,
          }}
        >
          仅删除关联关系
        </CheckBox>
        <CheckBox name="delete" value="deleteWithReply" onChange={handleChange} checked={deleteType === 'deleteWithReply'}>文件与关联关系一并删除</CheckBox>
      </div>
    </div>
  );
};

const ObserverDelete = observer(DeleteUI);

const openDeleteModal = (props: Props) => {
  Modal.open({
    key: 'deleteUI',
    title: '删除UI&UX文件',
    style: {
      width: 520,
    },
    className: styles.deleteModal,
    children: <ObserverDelete {...props} />,
  });
};
export default openDeleteModal;
