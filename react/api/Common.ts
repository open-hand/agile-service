import { axios, stores } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '@/utils/common';

const { AppState } = stores;
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
  getIssueReports(page: number = 1, param: string, userId: number | undefined) {
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
   * 
   */
  getIsShowFeature() {
    return axios.get(`${this.prefix}/art/isArtDoding`); // 多次请求待优化
  }


  /**
   * 查询项目群下的子项目  
   * @param onlySelectEnableSubProject  是否只查询启动的子项目
   */
  getSubProjects(onlySelectEnableSubProject: boolean = false) {
    return axios({
      method: 'get',
      url: `/iam/choerodon/v1/organizations/${getOrganizationId()}/project_relations/${getProjectId()}/${getProjectId()}`,
      params: {
        only_select_enable: onlySelectEnableSubProject || false,
      },
    });
  }

  /**
   * 查询当前项目是否在项目群中
   * 在项目群中返回项目群信息
   * 不在项目群中无返回信息
   */
  getProjectsInProgram() {
    return axios.get(`iam/choerodon/v1/organizations/${getOrganizationId()}/projects/${getProjectId()}/program`);
  }


  /**
   * 当前项目下用户拥有的角色
   */
  async getUserRolesInProject() {
    const userId = AppState.userInfo.id;
    const roles = await axios.get(`/iam/choerodon/v1/projects/${getProjectId()}/role_members/users/${userId}`);
    return roles.some((role: { code: string; [propName: string] : any }) => role.code === 'project-admin');
  }
}

const commonApi = new CommonApi();

export { commonApi };
