import { axios, stores } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '@/utils/common';

const { AppState } = stores;
class IssueLabelApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  /**
   * 查询当前项目下的Issue标签
   */
  loads(projectId?: string) {
    return axios.get(
      `/agile/v1/projects/${projectId || getProjectId()}/issue_labels`,
    );
  }
}

const issueLabelApi = new IssueLabelApi();

export { issueLabelApi };
