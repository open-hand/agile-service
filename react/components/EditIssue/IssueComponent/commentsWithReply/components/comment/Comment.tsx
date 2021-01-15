import React, { useCallback, useState, useContext } from 'react';
import { observer } from 'mobx-react-lite';
import './Comment.less';
import { IComment } from '@/common/types';
import { issueCommentApi, UComment } from '@/api/IssueComment';
import CommentItem, { ReplyComment } from './CommentItem';
import EditIssueContext from '../../../../stores';

interface Props {
  projectId?: string
  hasPermission: boolean
  comment: IComment
  onDelete?: Function
  reload: Function
}

const Comment: React.FC<Props> = (props) => {
  const { store } = useContext(EditIssueContext);
  const { commentExpandMap, commentReplysMap } = store;
  const { comment, reload } = props;

  const handleFold = useCallback(() => {
    commentExpandMap.set(comment.commentId, false);
  }, [comment.commentId, commentExpandMap]);

  const getReplys = useCallback((id?: string) => {
    issueCommentApi.getReplys(id || comment.commentId).then((res: ReplyComment[]) => {
      commentReplysMap.set(id || comment.commentId, res || []);
      commentExpandMap.set(id || comment.commentId, true);
    });
  }, [comment.commentId, commentExpandMap, commentReplysMap]);

  const onReply = useCallback((id?: string) => {
    const callback = () => {
      getReplys(id);
    };
    reload(callback);
  }, [getReplys, reload]);

  return (
    <div
      className="c7n-comment"
    >
      <div className="c7n-comment-self">
        <CommentItem
          isReply={false}
          {...props}
          onReply={onReply}
          onDelete={onReply}
          onUpdate={onReply}
          parentId={comment.commentId}
        />
        {
          commentExpandMap.get(comment.commentId) && (
            <div className="c7n-comment-replys">
              {
                (commentReplysMap.get(comment.commentId) || []).map((item: IComment) => (
                  <CommentItem
                    isReply
                    {...props}
                    onReply={onReply}
                    onDelete={onReply}
                    onUpdate={onReply}
                    reload={reload}
                    comment={item}
                    parentId={comment.commentId}
                  />
                ))
              }
            </div>
          )
        }
        {
          comment.replySize > 0 && (
          <div className="c7n-comment-expand">
            {
              commentExpandMap.get(comment.commentId) ? <span role="none" onClick={handleFold}>收起评论</span> : (
                <span role="none" onClick={() => { getReplys(); }}>
                  {`打开评论(${comment.replySize})`}
                </span>
              )
            }
          </div>
          )
        }
      </div>
    </div>
  );
};

export default observer(Comment);
