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
  loads() {
    return axios.get(
      `${this.prefix}/issue_labels`,
    );
  }
}

const issueLabelApi = new IssueLabelApi();

export { issueLabelApi };
