import React, { useCallback, useContext } from 'react';
import { observer } from 'mobx-react-lite';
import { stores, Permission } from '@choerodon/boot';
import './Comment.less';
import { IComment } from '@/common/types';
import { issueCommentApi } from '@/api/IssueComment';
import CommentItem, { ReplyComment } from './CommentItem';
import EditIssueContext from '../../../../stores';

interface Props {
  projectId?: string
  comment: IComment
  onDelete?: Function
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

const Comment: React.FC<Props> = (props) => {
  const { store } = useContext(EditIssueContext);
  const { commentExpandMap, commentReplysMap } = store;
  const { comment, reload, readonly } = props;
  const loginUserId = AppState.userInfo.id;

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

  const replyShows = commentExpandMap.get(comment.commentId) ? (commentReplysMap.get(comment.commentId) || []).reverse() : [
    {
      commentId: '215128304464850944',
      userId: '10635',
      issueId: '212667191353585664',
      commentText: '<p>1</p>',
      objectVersionNumber: 1,
      projectId: 1528,
      userName: '李文斐（20615）',
      userLoginName: '20615',
      userRealName: '李文斐',
      userImageUrl: 'https://minio.choerodon.com.cn/iam-service/file_219ce90e5973404ca351485453de2c8d_A63D7C242F3DAC79F718E11F56595B8F.jpg',
      lastUpdateDate: '2021-08-16 15:23:05',
      parentId: '215127955456815104',
      replyToUserId: '10635',
      replyToUserName: '李文斐（20615）',
      replyToUserLoginName: '20615',
      replyToUserRealName: '李文斐',
      replyToUserImageUrl: 'https://minio.choerodon.com.cn/iam-service/file_219ce90e5973404ca351485453de2c8d_A63D7C242F3DAC79F718E11F56595B8F.jpg',
      issueAttachmentVOList: null,
    },
    {
      commentId: '215128322148036608',
      userId: '10635',
      issueId: '212667191353585664',
      commentText: '<p>2</p>',
      objectVersionNumber: 1,
      projectId: 1528,
      userName: '李文斐（20615）',
      userLoginName: '20615',
      userRealName: '李文斐',
      userImageUrl: 'https://minio.choerodon.com.cn/iam-service/file_219ce90e5973404ca351485453de2c8d_A63D7C242F3DAC79F718E11F56595B8F.jpg',
      lastUpdateDate: '2021-08-16 15:23:09',
      parentId: '215127955456815104',
      replyToUserId: '10635',
      replyToUserName: '李文斐（20615）',
      replyToUserLoginName: '20615',
      replyToUserRealName: '李文斐',
      replyToUserImageUrl: 'https://minio.choerodon.com.cn/iam-service/file_219ce90e5973404ca351485453de2c8d_A63D7C242F3DAC79F718E11F56595B8F.jpg',
      issueAttachmentVOList: null,
    },
  ];
  return (
    <div
      className="c7n-comment"
    >
      <Permission
        service={['choerodon.code.project.cooperation.iteration-plan.ps.choerodon.code.agile.project.editissue.pro']}
      >
        {
            (hasPermission: boolean) => (
              <div className="c7n-comment-self">
                <CommentItem
                  isReply={false}
                  {...props}
                  onReply={onReply}
                  onDelete={onReply}
                  onUpdate={onReply}
                  parentId={comment.commentId}
                  hasPermission={hasPermission || String(comment.userId) === String(loginUserId)}
                  readonly={readonly}
                />
                <div className="c7n-comment-replys">
                  {
                    replyShows.map((item: IComment) => (
                      <CommentItem
                        isReply
                        {...props}
                        onReply={onReply}
                        onDelete={onReply}
                        onUpdate={onReply}
                        reload={reload}
                        comment={item}
                        parentId={comment.commentId}
                        hasPermission={hasPermission || String(item.userId) === String(loginUserId)}
                        readonly={readonly}
                      />
                    ))
                  }
                </div>
                {
                  comment.replySize > 2 && (
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
            )
          }
      </Permission>
    </div>
  );
};

export default observer(Comment);
