import { axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '../common/utils';

class FeatureApi {
  constructor() {

  }
  queryAllInSubProject(featureIds: number[], param: string, page: number = 1) {
    return axios.post(
      `/agile/v1/projects/${getProjectId()}/issues/feature/all`,
      featureIds || [],
      {
        params: {
          organizationId: getOrganizationId(),
          page,
          size: 10,
          param: param
        }
      }
    );
  }
}

const featureApi = new FeatureApi()
export { featureApi };
