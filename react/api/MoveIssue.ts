import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import Api from './Api';

class MoveIssueApi extends Api<MoveIssueApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  getProjectListMoveTo() {
    return this.request({
      method: 'get',
      url: '/iam/choerodon/v1/organizations/7/users/10635/projects/paging?page=0&size=0',
    });
  }
}

const moveIssueApi = new MoveIssueApi();
export { moveIssueApi };
