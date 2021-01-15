import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import Api from './Api';

export interface IComment {
    issueId: string,
    commentText: string,
}
export interface UComment {
  commentId: string
  objectVersionNumber:number,
  commentText: string,
}

export interface IReplyCreate {
  issueId: string,
  parentId: string
  replyToUserId: string
  commentText: string,
}
class IssueCommentApi extends Api<IssueCommentApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  /**
     * 创建issue评论
     * @param commitObj
     */
  create(commitObj: IComment) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/issue_comment`,
      data: commitObj,
    });
  }

  /**
   * 更新issue的评论
   * @param commitObj
   */
  update(commitUpdateObj:UComment) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/issue_comment/update`,
      data: commitUpdateObj,
    });
  }

  /**
   * 根据commitId删除评论, 单条
   * @param commitId
   */
  delete(commitId:string) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/issue_comment/${commitId}`,
    });
  }

  deleteWithReply(commentId: string) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/issue_comment/reply/${commentId}`,
    });
  }

  createReply(data: IReplyCreate) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/issue_comment/reply`,
      data,
    });
  }

  getReplys(commentId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/issue_comment/reply/${commentId}`,
    });
  }
}

const issueCommentApi = new IssueCommentApi();
export { issueCommentApi };
