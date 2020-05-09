import { axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '../common/utils';

class FeatureApi {
  constructor() {

  }
  /**
   * 在子项目查询所有特性
   * @param featureIds 接口额外要返回的特性
   * @param param 搜索
   * @param page 第几页
   */
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
