import React, { useState, useContext } from 'react';
import { Button, Icon } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import { text2Delta, beforeTextUpload } from '@/utils/richText';
import { issueCommentApi } from '@/api/IssueComment';
import WYSIWYGEditor from '../../../WYSIWYGEditor';
import Comment from '../../Component/Comment';
import EditIssueContext from '../../stores';

function IssueCommit({
  disabled, reloadIssue, store, loginUserId, hasPermission, projectId, outside,
}) {
  const { applyType } = useContext(EditIssueContext);
  const [addCommit, setAddCommit] = useState(false);
  const [addCommitDes, setAddCommitDes] = useState('');
  const [commentExpendAll, setCommentExpendAll] = useState(false);
  const delta = text2Delta(addCommitDes);
  const newCommit = (commit) => {
    const { issueId } = store.getIssue;
    issueCommentApi.project(projectId).create(commit).then(() => {
      if (reloadIssue) {
        reloadIssue(issueId);
      }
      setAddCommit(false);
      setAddCommitDes('');
    });
  };

  // 校验评论是否为空
  function verifyComment(comment) {
    let result = false;
    if (comment && comment.length) {
      comment.forEach((item) => {
        if (!result && item.insert && (item.insert.image || item.insert.trim())) {
          result = true;
        }
      });
    }
    return result;
  }

  const handleCreateCommit = () => {
    const issue = store.getIssue;
    const { issueId } = issue;
    if (addCommitDes && verifyComment(addCommitDes)) {
      beforeTextUpload(addCommitDes, { issueId, commentText: '' }, newCommit, 'commentText');
    } else {
      setAddCommit(false);
      setAddCommitDes('');
    }
  };

  const renderCommits = () => {
    const issue = store.getIssue;
    const { issueCommentVOList = [], issueId } = issue;
    return (
      <div style={{ marginBottom: 15 }}>
        {
          issueCommentVOList.map((comment, i) => (
            <Comment
              key={comment.commentId}
              projectId={projectId}
              comment={comment}
              onDeleteComment={() => { reloadIssue(issueId); }}
              onUpdateComment={() => { reloadIssue(issueId); }}
              loginUserId={loginUserId}
              hasPermission={hasPermission}
              i={i}
              commentExpendAll={commentExpendAll}
              disabled={disabled}
              applyType={applyType}
            />
          ))
        }
        {
          issueCommentVOList.length > 5 && !commentExpendAll ? (
            <div style={{ marginTop: 5 }}>
              <Button className="leftBtn" funcType="flat" onClick={() => setCommentExpendAll(true)}>
                <span>展开</span>
                <Icon type="baseline-arrow_right icon" style={{ marginRight: 2 }} />
              </Button>
            </div>
          ) : null
        }
        {
          issueCommentVOList.length > 5 && commentExpendAll ? (
            <div style={{ marginTop: 5 }}>
              <Button className="leftBtn" funcType="flat" onClick={() => setCommentExpendAll(false)}>
                <span>折叠</span>
                <Icon type="baseline-arrow_drop_up icon" style={{ marginRight: 2 }} />
              </Button>
            </div>
          ) : null
        }
      </div>
    );
  };
  return (
    <div id="commit" style={{ display: 'flex', flexDirection: 'column', minHeight: '100%' }}>
      {(!disabled || (disabled && applyType === 'agile' && !outside)) && (
        <div style={{ marginBottom: 10 }}>
          {
            addCommit ? (
              <div className="line-start mt-10" style={{ width: '100%' }}>
                <WYSIWYGEditor
                  autoFocus
                  bottomBar
                  value={delta}
                  style={{ height: 200, width: '100%' }}
                  onChange={(value) => {
                    setAddCommitDes(value);
                  }}
                  handleDelete={() => {
                    setAddCommit(false);
                    setAddCommitDes('');
                  }}
                  handleSave={handleCreateCommit}
                />
              </div>
            ) : (
              <div
                role="none"
                onClick={() => setAddCommit(true)}
                style={{
                  background: 'rgba(0,0,0,0.03)',
                  border: '1px solid rgba(0,0,0,0.20)',
                  borderRadius: '5px',
                  height: 36,
                  lineHeight: '32px',
                  width: '100%',
                  color: 'rgba(0,0,0,0.65)',
                  paddingLeft: 10,
                  cursor: 'pointer',
                }}
              >
                点击添加评论…
              </div>
            )
          }
        </div>
      )}
      {renderCommits()}
    </div>
  );
}

export default observer(IssueCommit);
