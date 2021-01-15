import React, { useCallback, useEffect, useState } from 'react';
import { Icon, Popconfirm } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import { text2Delta, beforeTextUpload } from '@/utils/richText';
import WYSIWYGEditor from '@/components/WYSIWYGEditor';
import WYSIWYGViewer from '@/components/WYSIWYGViewer';
import DatetimeAgo from '@/components/CommonComponent/DatetimeAgo';
import UserHead from '@/components/UserHead';
// @ts-ignore
import Delta from 'quill-delta';
import './Comment.less';
import { IComment } from '@/common/types';
import { issueCommentApi, UComment, IReplyCreate } from '@/api/IssueComment';
import openDeleteModal from '../deleteComment';

export interface ReplyComment extends IComment {
  replyToUserId: string
  replyToUserName: string
  replyToUserRealName: string
  replyToUserLoginName: string
  replyToUserImageUrl: string
}

interface Props {
  projectId?: string
  hasPermission: boolean
  comment: IComment | ReplyComment
  onDelete: Function
  onUpdate: Function
  onReply: Function
  isReply: boolean
  parentId: string
  reload: Function
}

const CommentItem: React.FC<Props> = ({
  hasPermission, comment, onDelete, onUpdate, onReply, projectId, isReply, parentId, reload,
}) => {
  const [editing, setEditing] = useState(false);
  const [value, setValue] = useState<Delta>();
  const [replying, setReplying] = useState(false);
  const [replyValue, setReplyValue] = useState<Delta>();
  useEffect(() => {
    const delta = text2Delta(comment.commentText);
    setValue(delta);
  }, [comment.commentText]);

  // 校验评论是否为空
  function verifyComment(delta: Delta) {
    let result = false;
    if (delta && delta.length) {
      delta.forEach((item:any) => {
        // @ts-ignore
        if (!result && item.insert && (item.insert.image || item.insert.trim())) {
          result = true;
        }
      });
    }
    return result;
  }

  const newReply = useCallback((commit: IReplyCreate) => {
    issueCommentApi.project(projectId).createReply(commit).then(() => {
      if (onReply) {
        onReply();
      }
    });
  }, [onReply, projectId]);

  const handleReply = useCallback(async () => {
    if (replyValue && verifyComment(replyValue)) {
      try {
        beforeTextUpload(replyValue, {
          issueId: comment.issueId, parentId, replyToUserId: comment.userId, commentText: '',
        }, newReply, 'commentText');
      } finally {
        setReplying(false);
        setReplyValue(undefined);
      }
    } else {
      setReplying(false);
      setReplyValue(undefined);
    }
  }, [comment.issueId, comment.userId, newReply, parentId, replyValue]);

  const updateComment = useCallback((ucomment: UComment) => {
    issueCommentApi.project(projectId).update(ucomment).then(() => {
      setEditing(false);
      setValue(undefined);
      if (onUpdate) {
        onUpdate();
      }
    });
  }, [onUpdate, projectId]);

  const handleUpdate = useCallback(async (delta: Delta) => {
    const extra: UComment = {
      commentId: comment.commentId,
      objectVersionNumber: comment.objectVersionNumber,
      commentText: '',
    };
    if (value) {
      beforeTextUpload(value, extra, updateComment, 'commentText');
    } else {
      extra.commentText = '';
      updateComment(extra);
    }
    setEditing(false);
  }, [comment.commentId, comment.objectVersionNumber, updateComment, value]);

  const handleClickDltBtn = useCallback(() => {
    openDeleteModal({
      comment, isReply, projectId, onDelete, parentId, reload,
    });
  }, [comment, isReply, onDelete, parentId, projectId, reload]);

  const canEditOrDelete = hasPermission;

  const handleChange = useCallback((delta: Delta) => {
    setValue(delta);
  }, []);

  const handleClickReply = useCallback(() => {
    if (editing) {
      setEditing(false);
      const delta = text2Delta(comment.commentText);
      setValue(delta);
    }

    setReplying(true);
  }, [comment.commentText, editing]);

  const handleReplyChange = useCallback((delta: Delta) => {
    setReplyValue(delta);
  }, []);

  return (
    <>
      <div className={`c7n-comment-item  ${isReply ? 'c7n-comment-reply' : ''}`}>
        <div className="line-justify">
          <div className="c7n-title-commit" style={{ flex: 1 }}>
            <UserHead
            // @ts-ignore
              user={{
                id: comment.userId,
                name: comment.userName,
                realName: comment.userRealName,
                loginName: comment.userLoginName,
                avatar: comment.userImageUrl,
              }}
              color="#3f51b5"
            />
            {
              isReply && (
                <div className="c7n-title-commit-to">
                  <span style={{
                    marginRight: 12,
                    color: 'rgba(0, 0, 0, 0.65)',
                  }}
                  >
                    回复
                  </span>
                  <UserHead
                    user={{
                      // @ts-ignore
                      id: comment.replyToUserId,
                      // @ts-ignore
                      name: comment.replyToUserName,
                      // @ts-ignore
                      realName: comment.replyToUserRealName,
                      // @ts-ignore
                      loginName: comment.replyToUserLoginName,
                      // @ts-ignore
                      avatar: comment.replyToUserImageUrl,
                    }}
                    color="#3f51b5"
                  />
                </div>
              )
            }
            <div style={{ color: 'rgba(0, 0, 0, 0.65)', marginLeft: 15 }}>
              <DatetimeAgo
                date={comment.lastUpdateDate}
              />
            </div>
          </div>
          <div className="c7n-action">
            <Icon type="message_notification" onClick={handleClickReply} />
            {
            hasPermission && (
              <Icon
                type="mode_edit mlr-3 pointer"
                onClick={() => {
                  if (canEditOrDelete) {
                    if (replying) {
                      setReplying(false);
                      setReplyValue(undefined);
                    }
                    setEditing(true);
                  }
                }}
              />
            )
          }

            {
              hasPermission && (
                <Icon
                  type="delete_forever mlr-3 pointer"
                  onClick={handleClickDltBtn}
                />
              )
            }
          </div>
        </div>
        {
            editing ? (
              <div className="c7n-conent-commit" style={{ marginTop: 10 }}>
                <WYSIWYGEditor
                  autoFocus
                  bottomBar
                  value={value}
                  onChange={handleChange}
                  style={{ height: 200, width: '100%' }}
                  handleDelete={() => {
                    setEditing(false);
                  }}
                  handleSave={handleUpdate}
                />
              </div>
            ) : (
              <div style={{ marginTop: 10, paddingLeft: 23 }}>
                <WYSIWYGViewer data={comment.commentText} />
              </div>
            )
          }
      </div>
      {
        replying && (
          <div className="c7n-comment-reply">
            <WYSIWYGEditor
              autoFocus
              bottomBar
              value={replyValue}
              onChange={handleReplyChange}
              style={{ height: 200, width: '100%' }}
              handleDelete={() => {
                setReplying(false);
                setReplyValue(undefined);
              }}
              handleSave={handleReply}
              saveText="回复"
            />
          </div>
        )
      }
    </>
  );
};

export default observer(CommentItem);
