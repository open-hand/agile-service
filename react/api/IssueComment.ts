import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

interface IComment {
    issueId: number,
    commentText: string,
}
interface UComment extends IComment {
    objectVersionNumber:number,
}
class IssueCommentApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  /**
     * 创建issue评论
     * @param commitObj 
     */
  create(commitObj: IComment) {
    return axios.post(`${this.prefix}/issue_comment`, commitObj);
  }


  /**
   * 更新issue的评论
   * @param commitObj 
   */
  update(commitUpdateObj:UComment) {
    return axios.post(`${this.prefix}/issue_comment/update`, commitUpdateObj);
  }

  /**
   * 根据commitId删除评论
   * @param commitId 
   */
  delete(commitId:number) {
    return axios.delete(`${this.prefix}/issue_comment/${commitId}`);
  }
}

const issueCommentApi = new IssueCommentApi();
export { issueCommentApi };
