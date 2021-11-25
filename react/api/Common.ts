import { axios, stores } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '@/utils/common';
import { IRole } from '@/common/types';
import Api from './Api';
import { withSelectedUsers } from './User';

const { AppState } = stores;
class CommonApi extends Api<CommonApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  /**
   * 查询项目所有报告人
   * @param page
   * @param param  查询词
   * @param userIds
   */
  getIssueReports(page: number = 1, param: string, userIds?: string[]) {
    return withSelectedUsers(axios({
      method: 'post',
      url: `${this.prefix}/issues/reporters`,
      params: {
        page,
        param,
      },
      data: userIds,
    }), {
      userIds,
      page,
      projectId: this.projectId,
    });
  }

  /**
   * 查询此项目是否展示特性字段
   *
   */
  getIsShowFeature() {
    return this.request({
      method: 'get',
      url: `${this.prefix}/art/isArtDoding`,
    });
  }

  /**
   * 查询项目群下的子项目
   * @param onlySelectEnableSubProject  是否只查询启动的子项目
   */
  getSubProjects(onlySelectEnableSubProject: boolean = false, projectId?: string) {
    return this.request({
      method: 'get',
      url: `/iam/choerodon/v1/organizations/${getOrganizationId()}/project_relations/${projectId || getProjectId()}/${projectId || getProjectId()}`,
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
  getProjectsInProgram(projectId?: string) {
    return this.request({
      method: 'get',
      url: `iam/choerodon/v1/organizations/${this.orgId}/projects/${projectId || getProjectId()}/program`,
      // cache: true,
    });
  }

  /**
   * 当前项目下用户拥有的角色
   */
  async getUserRolesInProject() {
    const userId = AppState.userInfo.id;
    const roles = await axios.get(`/iam/choerodon/v1/projects/${getProjectId()}/role_members/users/${userId}`);
    return roles.some((role: { code: string;[propName: string]: any }) => role.code === 'project-admin');
  }

  /**
   * 根据type code查询其下的value值
   */
  loadLookupValue(typeCode: string) {
    return axios.get(`/agile/v1/lookup_values/${typeCode}`);
  }

  /**
   *根据项目id检查用户是否有权限访问
   * @param projectId
   */
  checkProjectViewPermission(projectId: string | number) {
    return axios.get(`iam/choerodon/v1/projects/check-permission/${projectId}`);
  }

  /**
 * 获取角色列表
 * @param {boolean} enable 是否启用 @default true
 * @param {string} roleName 角色名
 * @param {string|number} projectId 项目 @default 当前项目
 * @returns {Array} 角色列表数据
 */
  getRoles(roleName: string = '', enable = true, projectId?: string | number): Promise<IRole[]> {
    return this.request({
      method: 'get',
      url: `iam/choerodon/v1/projects/${projectId || getProjectId()}/roles`,
      params: {
        only_select_enable: enable,
        role_name: roleName || '',
      },
    });
  }
}

const commonApi = new CommonApi();
const commonApiConfig = new CommonApi(true);

export { commonApi, commonApiConfig };
