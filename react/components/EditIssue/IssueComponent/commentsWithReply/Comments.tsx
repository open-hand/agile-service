import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import { stores } from '@choerodon/boot';
import { Button, Icon } from 'choerodon-ui/pro';
import Delta from 'quill-delta';
import { beforeTextUpload } from '@/utils/richText';
import { issueCommentApi, IComment } from '@/api/IssueComment';
import Comment from './components/comment';
import AddComment from './components/AddComment';
import EditIssueContext from '../../stores';

import styles from './Comments.less';

const EXPAND_NUM = 5;
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
  const { store, hasAdminPermission, applyType } = useContext(EditIssueContext);
  const { issueId, issueCommentVOList = [] } = store.issue;
  const loginUserId = AppState.userInfo.id;
  const comments = issueCommentVOList;

  const newCommit = (commit: IComment) => {
    issueCommentApi.project(projectId).create(commit).then(() => {
      if (reloadIssue) {
        reloadIssue(issueId);
      }
    });
  };

  const handleCreateCommit = async (delta: Delta) => {
    beforeTextUpload(delta, { issueId, commentText: '' }, newCommit, 'commentText');
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
              hasPermission={hasAdminPermission || String(comment.userId) === String(loginUserId)}
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
