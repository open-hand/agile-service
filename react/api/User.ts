import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

class UserApi {
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
  getById(userId: number) {
    return axios.get(`${this.prefix}/users?id=${userId}`);
  }

  /**
   * 在项目层查询用户列表（不包括离职用户）
   * @param param 模糊搜索
   * @param page 
   * @param id 根据id查询
   */
  getAllInProject(param?: string, page?: number, userId?: number) {
    return axios.get(`${this.prefix}/users`, {
      params: {
        param,
        id: userId,
        page: page || 1,
        size: 20,
      },
    });
  }

  /**
   * 在项目层查询用户列表（包括离职用户）
   * @param param 
   * @param page 
   * @param userId 
   */
  getAllInProjectIncludesLeaveUsers(param?: string, page?: number, userId?: number) {
    return axios.get(`/agile/v1/projects/${getProjectId()}/issues/users`, {
      params: {
        param,
        id: userId,
        page: page || 1,
        size: 20,
      },
    });
  }
}

const userApi = new UserApi();

export { userApi };
