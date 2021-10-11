import { axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '@/utils/common';
import Api from './Api';

interface IIssueRelation {
  issueId:number,
  wikiName: string,
  wikiUrl: string,
  projectId: number,
  spaceId: number,
}
class KnowledgeApi extends Api<KnowledgeApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  /**
   * 创建工作项关联的知识关系
   * @param data
   */
  createRelationForIssue(data:Array<IIssueRelation>) {
    return axios.post(`${this.prefix}/knowledge_relation`, data);
  }

  /**
   * 根据工作项id加载知识
   * @param issueId
   */
  loadByIssue(issueId: number) {
    return this.request({
      url: `${this.prefix}/knowledge_relation/issue/${issueId}`,
      method: 'get',
    });
  }

  /**
   * 查询当前项目下可用的知识库中的全部文档
   */
  loadAllCurrentProject() {
    const organizationId = getOrganizationId();
    return axios({
      method: 'get',
      url: `/knowledge/v1/projects/${getProjectId()}/work_space/all_space`,
      params: {
        organizationId,
      },
    });
  }

  /**
   * 删除工作项中的知识关联
   * @param relationId
   */
  deleteRelationForIssue(relationId:number) {
    return axios.delete(`${this.prefix}/knowledge_relation/${relationId}`);
  }
}

const knowledgeApi = new KnowledgeApi();
export { knowledgeApi };
