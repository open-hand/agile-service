import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import Api from './Api';

class MoveIssueApi extends Api<MoveIssueApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  getFieldsLosed(targetProject: string, issueId: string, typeCode: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/project_move/list_lost_field`,
      params: {
        targetProject,
        typeCode,
        issueId,
      },
    });
  }

  getProjectListMoveTo(typeCode: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/project_move/list_move_projects`,
      params: {
        typeCode,
      },
    });
  }

  moveIssueToProject(issueId: string, targetProjectId: string, data: any) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/project_move/${issueId}`,
      params: {
        targetProjectId,
      },
      data,
    });
  }
}

const moveIssueApi = new MoveIssueApi();
export { moveIssueApi };
