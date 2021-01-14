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
  const [expand, setExpand] = useState(false);
  const [replys, setReplys] = useState<IComment[]>([]);

  const handleFold = useCallback(() => {
    // setExpand(false);
    commentExpandMap.set(comment.commentId, false);
  }, [comment.commentId, commentExpandMap]);

  const getReplys = useCallback(() => {
    // setExpand(true);
    commentExpandMap.set(comment.commentId, true);
    issueCommentApi.getReplys(comment.commentId).then((res: ReplyComment[]) => {
      // setReplys(res);
      commentReplysMap.set(comment.commentId, res || []);
    });
  }, [comment.commentId, commentExpandMap, commentReplysMap]);

  const onReply = useCallback(() => {
    const callback = () => {
      // commentReplysMap.clear();
      // commentExpandMap.clear();
      getReplys();
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
          onDelete={reload}
          onUpdate={reload}
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
                    onReply={reload}
                    onDelete={reload}
                    onUpdate={reload}
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
                <span role="none" onClick={getReplys}>
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
