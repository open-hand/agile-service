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
  status: 'success' | 'failed'
  process: 1
}

interface Props {
  ui: IUi
  store: EditIssueStore
  modal?: IModalProps
  reloadIssue: Function
}
const DeleteUI: React.FC<Props> = ({
  ui, store, modal, reloadIssue,
}) => {
  const { issue } = store;
  const [deleteType, setDeleteType] = useState('deleteLinkOnly');
  const handleChange = useCallback((value) => {
    if (value) {
      setDeleteType(value);
    } else {
      setDeleteType(deleteType === 'deleteWithLink' ? 'deleteLinkOnly' : 'deleteWithLink');
    }
  }, [deleteType]);

  const handleDelete = useCallback(() => {
    if (deleteType === 'deleteWithLink') {
      return uiApi.project(store.projectId).deleteUI(ui.id).then(() => {
        store.getLinkedUI();
        reloadIssue();
      });
    }
    // @ts-ignore
    return uiApi.project(store.projectId).deleteLink(issue.issueId, ui.id).then(() => {
      store.getLinkedUI();
      reloadIssue();
    });
    // @ts-ignore
  }, [deleteType, issue.issueId, reloadIssue, store, ui.id]);

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
          value="deleteLinkOnly"
          onChange={handleChange}
          checked={deleteType === 'deleteLinkOnly'}
          style={{
            marginRight: 10,
          }}
        >
          仅删除关联关系
        </CheckBox>
        <CheckBox name="delete" value="deleteWithLink" onChange={handleChange} checked={deleteType === 'deleteWithLink'}>文件与关联关系一并删除</CheckBox>
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
