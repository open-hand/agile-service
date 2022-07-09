import React, {
  useCallback, useEffect, useImperativeHandle, useState,
} from 'react';
import { Icon } from 'choerodon-ui/pro';
import { stores } from '@choerodon/boot';
import { observer } from 'mobx-react-lite';
import WYSIWYGEditor from '@/components/CKEditor';
import WYSIWYGViewer from '@/components/CKEditorViewer';
import './Comment.less';
import { IComment } from '@/common/types';
import { IReplyCreate, issueCommentApi, UComment } from '@/api/IssueComment';
import UserTag from '@/components/tag/user-tag';
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
  readonly: boolean
  addingRef: React.MutableRefObject<{
    adding: boolean,
    setAdding: (adding: boolean) => void
    setAddValue: (v: string) => void
  } | null>
  editingRef: React.MutableRefObject<{
    editing: boolean,
    setEditing: (editing: boolean) => void
    setEditValue: (v: string) => void
    initValue: string
  } | null>
  replyingRef: React.MutableRefObject<{
    replying: boolean,
    setReplying:(replying: boolean) => void
    setReplyValue: (v: string) => void
      } | null>
}
const { AppState } = stores;

const CommentItem: React.FC<Props> = ({
  hasPermission, comment, onDelete, onUpdate, onReply, projectId, isReply, parentId, reload, readonly, addingRef, editingRef, replyingRef,
}) => {
  const loginUserId = AppState.userInfo.id;
  const isSelf = String(comment.userId) === String(loginUserId);
  const [editing, setEditing] = useState(false);
  const [value, setValue] = useState<string>('');
  const [replying, setReplying] = useState(false);
  const [replyValue, setReplyValue] = useState<string>('');
  useEffect(() => {
    const delta = comment.commentText;
    setValue(delta);
  }, [comment.commentText]);

  // 校验评论是否为空
  function verifyComment(delta: string) {
    return delta.length > 0;
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
        const commentText = replyValue;
        newReply({
          // @ts-ignore
          issueId: comment.issueId,
          parentId,
          // @ts-ignore
          replyToUserId: comment.userId,
          commentText,
        });
        setReplying(false);
        setReplyValue('');
      } catch {
        //
      }
    } else {
      setReplying(false);
      setReplyValue('');
    }
  }, [comment.issueId, comment.userId, newReply, parentId, replyValue]);

  const updateComment = useCallback((ucomment: UComment) => {
    issueCommentApi.project(projectId).update(ucomment, isSelf).then(() => {
      setEditing(false);
      if (onUpdate) {
        onUpdate();
      }
    });
  }, [isSelf, onUpdate, projectId]);

  const handleUpdate = useCallback(async (delta: string) => {
    if (value && verifyComment(value)) {
      const commentText = value;
      updateComment({
        commentId: comment.commentId,
        objectVersionNumber: comment.objectVersionNumber,
        commentText,
      });
    } else {
      setValue(comment.commentText);
    }
    setEditing(false);
  }, [comment.commentId, comment.objectVersionNumber, updateComment, value]);

  const handleClickDltBtn = useCallback(() => {
    openDeleteModal({
      comment, isReply, projectId, onDelete, parentId, reload,
    });
  }, [comment, isReply, onDelete, parentId, projectId, reload]);

  const canEditOrDelete = hasPermission;

  const handleChange = useCallback((delta: string) => {
    setValue(delta);
  }, []);

  const handleClickReply = useCallback(() => {
    if (editing) {
      setEditing(false);
      const delta = comment.commentText;
      setValue(delta);
    }

    if (addingRef?.current?.adding) {
      addingRef?.current?.setAddValue('');
      addingRef?.current?.setAdding(false);
    }

    setReplying(true);
  }, [addingRef, comment.commentText, editing]);

  const handleReplyChange = useCallback((delta: string) => {
    setReplyValue(delta);
  }, []);

  useImperativeHandle(editingRef, () => ({
    editing,
    setEditing,
    setEditValue: setValue,
    initValue: comment.commentText,
  }));

  useImperativeHandle(replyingRef, () => ({
    replying,
    setReplyValue,
    setReplying,
  }));

  return (
    <>
      <div className={`c7n-comment-item  ${isReply ? 'c7n-comment-reply' : ''}`}>
        <div className="line-justify">
          <div className="c7n-title-commit" style={{ flex: 1 }}>
            <UserTag
              data={{
                // id: comment.userId,
                tooltip: comment.userName,
                realName: comment.userRealName,
                loginName: comment.userLoginName,
                imageUrl: comment.userImageUrl,
              }}
              textStyle={{ color: 'var(--primary-color)' }}
            />
            {
              isReply && (
                <div className="c7n-title-commit-to">
                  <span style={{
                    marginRight: 12,
                    color: 'var(--text-color3)',
                  }}
                  >
                    回复
                  </span>
                  <UserTag
                    data={{
                      // id: comment.replyToUserId,
                      tooltip: (comment as ReplyComment).replyToUserName,
                      realName: (comment as ReplyComment).replyToUserRealName,
                      loginName: (comment as ReplyComment).replyToUserLoginName,
                      imageUrl: (comment as ReplyComment).replyToUserImageUrl,
                    }}
                    textStyle={{ color: '#5365EA' }}
                  />
                </div>
              )
            }
            <div style={{ color: 'var(--text-color3)', marginLeft: 15 }}>
              {comment.lastUpdateDate}
            </div>
          </div>
          <div className="c7n-action">
            {
              !readonly && (
                <Icon type="sms-o" onClick={handleClickReply} />
              )
            }
            {
              !readonly && hasPermission && (
                <Icon
                  type="edit-o"
                  onClick={() => {
                    if (canEditOrDelete) {
                      if (replying) {
                        setReplying(false);
                        setReplyValue('');
                      }
                      if (addingRef?.current?.adding) {
                        addingRef?.current?.setAddValue('');
                        addingRef?.current?.setAdding(false);
                      }
                      setEditing(true);
                    }
                  }}
                />
              )
            }

            {
              !readonly && hasPermission && (
                <Icon
                  type="delete_sweep-o"
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
                footer
                value={value}
                onChange={handleChange}
                style={{ minHeight: 300, width: '100%' }}
                onCancel={() => {
                  setEditing(false);
                  setValue(comment.commentText);
                }}
                onOk={handleUpdate}
                projectId={projectId}
                placeholder="请输入评论"
              />
            </div>
          ) : (
            <div style={{ marginTop: 10, paddingLeft: 23 }}>
              <WYSIWYGViewer value={comment.commentText} />
            </div>
          )
        }
      </div>
      {
        replying && (
          <div className="c7n-comment-reply">
            <WYSIWYGEditor
              autoFocus
              footer
              value={replyValue}
              onChange={handleReplyChange}
              style={{ minHeight: 300, width: '100%' }}
              onCancel={() => {
                setReplying(false);
                setReplyValue('');
              }}
              onOk={handleReply}
              okText="回复"
              projectId={projectId}
              placeholder={`回复 ${comment.userRealName}:`}
            />
          </div>
        )
      }
    </>
  );
};

export default observer(CommentItem);
