import React, {
  useContext, useRef, useState, useCallback,
} from 'react';
import { Icon } from 'choerodon-ui';
import { Button } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { useSize } from 'ahooks';
import { issueCommentApi, IComment } from '@/api/IssueComment';
import Comment from './components/comment';
import AddComment from './components/addComment';
import EditIssueContext from '../../stores';

import styles from './Comments.less';
import { issueApi } from '@/api';

interface Props {
  projectId: string
  reloadIssue: Function
  disabled: string
  outside: string
}

const Comments: React.FC<Props> = ({
  reloadIssue, disabled,
}) => {
  const [folded, setFolded] = useState<boolean>(true);
  const [page, setPage] = useState<number>(1);
  const commentsRef = useRef<HTMLDivElement | null>(null);
  const commentsSize = useSize(commentsRef);
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

  const {
    store, outside,
    projectId,
    organizationId,
    applyType,
    programId,
    issueId: id,
  } = useContext(EditIssueContext);
  const { comments } = store;
  const { issueId } = store.issue;

  const getMoreComments = useCallback(async () => {
    const res = programId ? await issueApi.project(projectId).loadUnderProgram(id, programId) : await issueApi.org(organizationId).outside(outside).project(projectId).load(id);
    const newComments = {
      ...res,
      content: [...(comments.content || []), ...(res.content || [])],
    };
    setFolded(false);
    store.setComments(newComments);
  }, [comments?.content, id, organizationId, outside, programId, projectId, store]);

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

  const handleFold = useCallback(() => {
    setFolded(true);
  }, []);

  const readonly = !(!disabled || (disabled && applyType === 'agile' && !outside));

  return (
    <div className={styles.comments} ref={commentsRef}>
      <div className={styles.list}>
        {
          (comments?.content || []).slice(0, folded ? 10 : (comments?.content?.length || 10)).map((comment: any) => (
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
        {
            comments.totalPages > 1 && (
            <div className="c7n-comment-expand">
              {
                comments.totalPages === comments.number + 1 ? (
                  <Button className="leftBtn" onClick={handleFold}>
                    <span>收起</span>
                    <Icon type="baseline-arrow_drop_up icon" style={{ marginRight: 2 }} />
                  </Button>
                ) : (
                  <Button className="leftBtn" onClick={getMoreComments}>
                    <span>展开</span>
                    <Icon type="baseline-arrow_right icon" style={{ marginRight: 2 }} />
                  </Button>
                )
              }
            </div>
            )
        }
      </div>
      {
        (!disabled || !readonly) && (
          <div className={styles.add}>
            <AddComment onSubmit={handleCreateCommit} addingRef={addingRef} editingRef={editingRef} replyingRef={replyingRef} commentsHeight={commentsSize.height} />
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
