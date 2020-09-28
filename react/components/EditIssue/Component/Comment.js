import React, { Component } from 'react';
import { Icon, Popconfirm } from 'choerodon-ui';
import {
  text2Delta, beforeTextUpload,
} from '@/utils/richText';
import { issueCommentApi } from '@/api/IssueComment';
import UserHead from '../../tag/user';
import WYSIWYGEditor from '../../WYSIWYGEditor';
import WYSIWYGViewer from '../../WYSIWYGViewer';
import { DatetimeAgo } from '../../CommonComponent';
import './Comment.less';

class Comment extends Component {
  constructor(props, context) {
    super(props, context);
    this.state = {
      editCommentId: undefined,
      editComment: undefined,
      expand: true,
    };
  }

  componentDidMount() {
  }

  handleDeleteCommit = (commentId) => {
    const { onDeleteComment } = this.props;
    issueCommentApi.delete(commentId)
      .then(() => {
        if (onDeleteComment) {
          onDeleteComment();
        }
      });
  };

  handleUpdateComment = (comment) => {
    const { editComment } = this.state;
    const { commentId, objectVersionNumber } = comment;
    const extra = {
      commentId,
      objectVersionNumber,
    };
    const updateCommentDes = editComment;
    if (updateCommentDes) {
      beforeTextUpload(updateCommentDes, extra, this.updateComment, 'commentText');
    } else {
      extra.commentText = '';
      this.updateComment(extra);
    }
  };

  updateComment = (comment) => {
    const { onUpdateComment } = this.props;
    issueCommentApi.update(comment).then(() => {
      this.setState({
        editCommentId: undefined,
        editComment: undefined,
      });
      if (onUpdateComment) {
        onUpdateComment();
      }
    });
  };

  render() {
    const {
      comment, loginUserId, hasPermission, i, commentExpendAll,
    } = this.props;
    const { editComment, editCommentId, expand } = this.state;
    const canEditOrDelete = (comment && comment.userId === loginUserId) || hasPermission;
    const deltaEdit = text2Delta(editComment);
    return (
      <>
        {
          i > 4 && !commentExpendAll ? null : (
            <div
              className={`c7n-comment ${comment.commentId === editCommentId ? 'c7n-comment-focus' : ''}`}
            >
              <div className="line-justify">
                {/* {
                expand ? (
                  <Icon
                    role="none"
                    style={{
                      position: 'absolute',
                      left: 5,
                      top: 15,
                    }}
                    type="baseline-arrow_drop_down pointer"
                    onClick={() => {
                      this.setState({
                        expand: false,
                      });
                    }}
                  />
                ) : null
              }
                {
                !expand ? (
                  <Icon
                    role="none"
                    style={{
                      position: 'absolute',
                      left: 5,
                      top: 15,
                    }}
                    type="baseline-arrow_right pointer"
                    onClick={() => {
                      this.setState({
                        expand: true,
                      });
                    }}
                  />
                ) : null
              } */}
                <div className="c7n-title-commit" style={{ flex: 1 }}>
                  <UserHead
                    data={{
                      id: comment.userId,
                      name: comment.userName,
                      realName: comment.userRealName,
                      loginName: comment.userLoginName,
                      avatar: comment.userImageUrl,
                    }}
                    color="#3f51b5"
                  />
                  <div style={{ color: 'rgba(0, 0, 0, 0.65)', marginLeft: 15 }}>
                    <DatetimeAgo
                      date={comment.lastUpdateDate}
                    />
                  </div>
                </div>
                <div className="c7n-action">
                  <Icon
                    role="none"
                    type="mode_edit mlr-3 pointer"
                    style={{ cursor: canEditOrDelete ? 'pointer' : 'auto', color: canEditOrDelete ? '#000' : 'rgba(0, 0, 0, 0.25)' }}
                    onClick={() => {
                      if (canEditOrDelete) {
                        this.setState({
                          editCommentId: comment.commentId,
                          editComment: comment.commentText,
                          expand: true,
                        });
                      }
                    }}
                  />
                  {
                    canEditOrDelete ? (
                      <Popconfirm
                        title="确认要删除该评论吗?"
                        placement="left"
                        onConfirm={() => this.handleDeleteCommit(comment.commentId)}
                        onCancel={this.cancel}
                        okText="删除"
                        cancelText="取消"
                        okType="danger"
                      >
                        <Icon
                          role="none"
                          type="delete_forever mlr-3 pointer"
                        />
                      </Popconfirm>
                    ) : (
                      <Icon
                        role="none"
                        type="delete_forever mlr-3 pointer"
                        style={{ cursor: 'auto', color: 'rgba(0, 0, 0, 0.25)' }}
                      />
                    )
                  }

                </div>
              </div>
              {
                expand && (
                  <div className="c7n-conent-commit" style={{ marginTop: 10 }}>
                    {
                      comment.commentId === editCommentId ? (
                        <WYSIWYGEditor
                          autoFocus
                          bottomBar
                          value={deltaEdit}
                          style={{ height: 200, width: '100%' }}
                          onChange={(value) => {
                            this.setState({ editComment: value });
                          }}
                          handleDelete={() => {
                            this.setState({
                              editCommentId: undefined,
                              editComment: undefined,
                            });
                          }}
                          handleSave={this.handleUpdateComment.bind(this, comment)}
                        // toolbarHeight={isWide ? null : 66}
                        // toolbarHeight={66}
                        />
                      ) : (
                        <WYSIWYGViewer data={comment.commentText} />
                      )
                    }
                  </div>
                )
              }
            </div>
          )
        }
      </>

    );
  }
}

export default Comment;
