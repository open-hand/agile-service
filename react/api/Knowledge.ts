import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

interface IIssueRelation {
  issueId:number,
  wikiName: string,
  wikiUrl: string,
  projectId: number,
  spaceId: number,
}
class KnowledgeApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  /**
   * 创建问题关联的知识关系
   * @param data 
   */
  createRelationForIssue(data:Array<IIssueRelation>) {
    return axios.post(`${this.prefix}/knowledge_relation`, data);
  }

  /**
   * 根据问题id加载知识
   * @param issueId 
   */
  loadByIssue(issueId: number) {
    return axios.get(`${this.prefix}/knowledge_relation/issue/${issueId}`);
  }

  /**
   * 删除问题中的知识关联
   * @param relationId 
   */
  deleteRelationForIssue(relationId:number) {
    return axios.delete(`${this.prefix}/knowledge_relation/${relationId}`);
  }
}

const knowledgeApi = new KnowledgeApi();
export { knowledgeApi };
