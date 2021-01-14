import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import { stores } from '@choerodon/boot';
import { Button, Icon } from 'choerodon-ui/pro';
import Delta from 'quill-delta';
import Comment from './components/comment';

import EditIssueContext from '../../stores';

import styles from './Comments.less';

const EXPAND_NUM = 5;
const { AppState } = stores;

interface Props {

}

const Comments: React.FC<Props> = () => {
  const { store, hasAdminPermission } = useContext(EditIssueContext);
  const { issueCommentVOList = [] } = store.issue;
  const loginUserId = AppState.userInfo.id;
  const comments = issueCommentVOList;
  console.log('xinderender');
  return (
    <div className={styles.comments}>
      {
        comments.reverse().map((comment: any) => (
          <Comment
            key={comment.id}
            comment={comment}
            onDelete={() => { store.deleteComment(comment.id); }}
            onUpdate={(delta) => store.updateComment(delta, comment)}
            hasPermission={hasAdminPermission || String(comment.userId) === String(loginUserId)}
          />
        ))
      }
    </div>
  );
};

export default observer(Comments);
