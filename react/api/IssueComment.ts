import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import Api from './Api';

interface IComment {
    issueId: number,
    commentText: string,
}
interface UComment extends IComment {
    objectVersionNumber:number,
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
   * 根据commitId删除评论
   * @param commitId
   */
  delete(commitId:number) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/issue_comment/${commitId}`,
    });
  }
}

const issueCommentApi = new IssueCommentApi();
export { issueCommentApi };
