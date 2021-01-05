import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import './Cache';
import { User } from '@/common/types';
import Api from './Api';

class UserApi extends Api<UserApi> {
  get prefix() {
    return `/iam/choerodon/v1/projects/${getProjectId()}`;
  }

  /**
   * 查询当前用户信息
   */
  getSelf() {
    return axios.get('/iam/choerodon/v1/users/self');
  }

  /**
 * 根据用户id查询用户信息
 * @param userId
 */
  getById(userId: number | string) {
    return axios.get(`${this.prefix}/users?id=${userId}`);
  }

  /**
   * 在项目层查询用户列表（不包括离职用户）
   * @param param 模糊搜索
   * @param page
   * @param id 根据id查询
   */
  getAllInProject(param?: string, page?: number, userId?: number, size?: number, projectId?: string): Promise<{
    list: User[]
    hasNextPage: boolean
  }> {
    return this.request({
      method: 'get',
      url: `/iam/choerodon/v1/projects/${projectId || getProjectId()}/users`,
      params: {
        param,
        id: userId,
        page: page || 1,
        size: size || 20,
      },
      cache: true,
    });
  }

  getAllInOrg(param?: string, page?: number, userId?: number, size?: number, projectId?: number): Promise<{
    list: User[]
    hasNextPage: boolean
  }> {
    return this.request({
      method: 'get',
      url: `/iam/choerodon/v1/organizations/${projectId || getProjectId()}/users`,
      params: {
        param,
        id: userId,
        page: page || 1,
        size: size || 20,
      },
      cache: true,
    });
  }

  /**
   * 在项目层查询用户列表（包括离职用户）
   * @param param
   * @param page
   * @param userId
   */
  getAllInProjectIncludesLeaveUsers(param?: string, page?: number, userId?: number) {
    return this.request({
      method: 'get',
      url: `/agile/v1/projects/${getProjectId()}/issues/users`,
      params: {
        param,
        id: userId,
        page: page || 1,
        size: 20,
      },
      cache: true,
    });
  }
}

const userApi = new UserApi();

export { userApi };
