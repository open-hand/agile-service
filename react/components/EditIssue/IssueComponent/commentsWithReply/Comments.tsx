import React, { useContext, useRef } from 'react';
import { observer } from 'mobx-react-lite';
import { issueCommentApi, IComment } from '@/api/IssueComment';
import Comment from './components/comment';
import AddComment from './components/addComment';
import EditIssueContext from '../../stores';

import styles from './Comments.less';

interface Props {
  projectId: string
  reloadIssue: Function
  disabled: string
  outside: string
}

const Comments: React.FC<Props> = ({
  projectId, reloadIssue, disabled, outside,
}) => {
  const addingRef = useRef<{
    adding: boolean,
    setAdding:(adding: boolean) => void
    setAddValue: (v: string) => void
      } | null>(null);

  const editingRef = useRef<{
    editing: boolean,
    setEditing:(editing: boolean) => void
    setEditValue: (v: string) => void
    initValue: string
      } | null>(null);

  const replyingRef = useRef<{
      replying: boolean,
      setReplying:(replying: boolean) => void
      setReplyValue: (v: string) => void
        } | null>(null);

  const { store, applyType } = useContext(EditIssueContext);
  const { issueId, issueCommentVOList = [] } = store.issue;
  const comments = issueCommentVOList;

  const newCommit = (commit: IComment) => {
    issueCommentApi.project(projectId).create(commit).then(() => {
      if (reloadIssue) {
        reloadIssue(issueId);
      }
    });
  };

  const handleCreateCommit = async (delta: string) => {
    const commentText = delta;
    newCommit({ issueId, commentText });
  };

  const reload = (callback: Function) => {
    if (reloadIssue) {
      reloadIssue(issueId, callback);
    }
  };

  const readonly = !(!disabled || (disabled && applyType === 'agile' && !outside));
  return (
    <div className={styles.comments}>
      <div className={styles.list}>
        {
          comments.map((comment: any) => (
            <Comment
              projectId={projectId}
              key={comment.commentId}
              comment={comment}
              reload={reload}
              readonly={readonly}
              addingRef={addingRef}
              editingRef={editingRef}
              replyingRef={replyingRef}
            />
          ))
        }
      </div>
      {
        (!disabled || !readonly) && (
          <div className={styles.add}>
            <AddComment onSubmit={handleCreateCommit} addingRef={addingRef} editingRef={editingRef} replyingRef={replyingRef} />
          </div>
        )
      }
      {
        readonly && !comments.length && (
          <span style={{ textAlign: 'center', color: 'var(--text-color3)' }}>暂无评论</span>
        )
      }
    </div>
  );
};

export default observer(Comments);
