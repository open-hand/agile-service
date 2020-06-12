import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

class CommonApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  /**
   * 查询项目所有报告人
   * @param page 
   * @param param  查询词
   * @param userId  后端不处理
   */
  getIssueReports(page:number = 1, param:string, userId:number | undefined) {
    return axios({
      method: 'get',
      url: `${this.prefix}/issues/reporters`,
      params: {
        page,
        // userId,
        param,
      },
    });
  }

  /**
   * 查询此项目是否展示特性字段
   */
  getIsShowFeature() { 
    return axios.get(`${this.prefix}/art/isArtDoding`);
  }
}

const commonApi = new CommonApi();
// eslint-disable-next-line import/prefer-default-export
export { commonApi };
