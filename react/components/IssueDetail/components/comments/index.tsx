import React, { useState } from 'react';
import { stores } from '@choerodon/boot';
import { Button, Icon } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
// @ts-ignore
import Delta from 'quill-delta';
import Comment from './Comment';
import AddComment from './AddComment';
import { useDetailContext } from '../../context';

const EXPAND_NUM = 5;
const { AppState } = stores;
const Comments: React.FC = () => {
  const { store, hasAdminPermission } = useDetailContext();
  const [expand, setExpand] = useState(false);
  const handleCreateCommit = async (delta: Delta) => {
    store.createComment(delta);
  };

  const renderCommits = () => {
    const { issueCommentVOList = [] } = store.issue;
    const loginUserId = AppState.userInfo.id;
    const comments = expand ? issueCommentVOList : issueCommentVOList.slice(0, EXPAND_NUM);
    return (
      <div style={{ marginBottom: 15 }}>
        {
          comments.reverse().map((comment) => (
            <Comment
              key={comment.commentId}
              comment={comment}
              onDelete={() => { store.deleteComment(comment.commentId); }}
              onUpdate={(delta) => store.updateComment(delta, comment)}
              hasPermission={hasAdminPermission || String(comment.userId) === String(loginUserId)}
            />
          ))
        }
        {
          issueCommentVOList.length > EXPAND_NUM && !expand ? (
            <div style={{ marginTop: 5 }}>
              <Button onClick={() => setExpand(true)}>
                <span>展开</span>
                <Icon type="baseline-arrow_right" style={{ marginRight: 2 }} />
              </Button>
            </div>
          ) : null
        }
        {
          issueCommentVOList.length > EXPAND_NUM && expand ? (
            <div style={{ marginTop: 5 }}>
              <Button onClick={() => setExpand(false)}>
                <span>折叠</span>
                <Icon type="baseline-arrow_drop_up" style={{ marginRight: 2 }} />
              </Button>
            </div>
          ) : null
        }
      </div>
    );
  };

  return (
    <div style={{ minHeight: '100%' }}>
      {/* <AddComment onSubmit={handleCreateCommit} /> */}
      {renderCommits()}
    </div>
  );
};

export default observer(Comments);
