import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import { stores } from '@choerodon/boot';
import Delta from 'quill-delta';
import { uploadAndReplaceImg } from '@/utils/richText';
import { issueCommentApi, IComment } from '@/api/IssueComment';
import Comment from './components/comment';
import AddComment from './components/addComment';
import EditIssueContext from '../../stores';

import styles from './Comments.less';

const { AppState } = stores;

interface Props {
  projectId: string
  reloadIssue: Function
  disabled: string
  outside: string
}

const Comments: React.FC<Props> = ({
  projectId, reloadIssue, disabled, outside,
}) => {
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

  const handleCreateCommit = async (delta: Delta) => {
    const commentText = await uploadAndReplaceImg(delta);
    newCommit({ issueId, commentText });
  };

  const reload = (callback: Function) => {
    if (reloadIssue) {
      reloadIssue(issueId, callback);
    }
  };

  return (
    <div className={styles.comments}>
      <div className={styles.list}>
        {
          comments.reverse().map((comment: any) => (
            <Comment
              projectId={projectId}
              key={comment.commentId}
              comment={comment}
              reload={reload}
            />
          ))
        }
      </div>
      {
        (!disabled || (disabled && applyType === 'agile' && !outside)) && (
          <div className={styles.add}>
            <AddComment onSubmit={handleCreateCommit} />
          </div>
        )
      }
    </div>
  );
};

export default observer(Comments);
