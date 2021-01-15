import React, { useCallback, useState, useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { Modal, CheckBox } from 'choerodon-ui/pro';
import { IComment, IModalProps } from '@/common/types';
import { issueCommentApi } from '@/api/IssueComment';
import styles from './DeleteComment.less';

interface Props {
  comment: IComment,
  isReply: boolean,
  projectId?: string,
  onDelete: Function,
  modal?: IModalProps,
}
const DeleteComment: React.FC<Props> = ({
  comment, isReply, modal, onDelete, projectId,
}) => {
  const [deleteType, setDeleteType] = useState('deleteWithReply');
  const handleChange = useCallback((value) => {
    if (value) {
      setDeleteType(value);
    } else {
      setDeleteType(deleteType === 'deleteWithReply' ? 'deleteOnly' : 'deleteWithReply');
    }
  }, [deleteType]);

  const handleDelete = useCallback(() => {
    if (deleteType === 'deleteWithReply') {
      return issueCommentApi.project(projectId).deleteWithReply(comment.commentId).then(() => {
        modal?.close();
        if (onDelete) {
          onDelete();
        }
      });
    }
    return issueCommentApi.project(projectId).delete(comment.commentId)
      .then(() => {
        modal?.close();
        if (onDelete) {
          onDelete();
        }
      });
  }, [comment.commentId, deleteType, modal, onDelete, projectId]);

  useEffect(() => {
    modal?.handleOk(handleDelete);
  }, [handleDelete, modal]);
  return (
    <div className={styles.deleteComment}>
      <p style={{
        marginBottom: 23,
      }}
      >
        {`确定要删除“${comment.userRealName}”的${isReply ? '回复' : '评论'}？删除后将无法恢复，请谨慎操作！`}
      </p>
      <div>
        <CheckBox name="delete" value="deleteWithReply" onChange={handleChange} checked={deleteType === 'deleteWithReply'}>删除评论以及回复</CheckBox>
        <CheckBox name="delete" value="deleteOnly" onChange={handleChange} checked={deleteType === 'deleteOnly'}>仅删除评论</CheckBox>
      </div>
    </div>
  );
};

const ObserverDelete = observer(DeleteComment);

const openDeleteModal = (props: Props) => {
  Modal.open({
    key: 'deleteComment',
    title: '删除评论',
    style: {
      width: 520,
    },
    className: styles.deleteModal,
    children: <ObserverDelete {...props} />,
  });
};
export default openDeleteModal;
