import { axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '@/utils/common';
import './Cache';
import { User } from '@/common/types';
import { unionBy } from 'lodash';
import Api from './Api';

export async function getProjectUsersByIds(userIds?: string[], projectId?: string): Promise<User[]> {
  return axios({
    method: 'post',
    url: `/agile/v1/users/projects/${projectId ?? getProjectId()}/list_by_ids`,
    data: userIds ?? [],
  });
}

export async function getOrgUsersByIds(userIds?: string[], orgId?: string): Promise<User[]> {
  return axios({
    method: 'post',
    url: `/agile/v1/users/organizations/${orgId ?? getOrganizationId()}/list_by_ids`,
    data: userIds ?? [],
  });
}
export async function withSelectedUsers(promise: Promise<{
  list: User[]
  hasNextPage: boolean
  number: number
}>, {
  userIds, page, isOrg, projectId,
}: {
  userIds?: string[]
  page: number
  isOrg?: boolean
  projectId?: string
}) {
  const pageUsers = await promise;
  if (userIds && userIds.length > 0 && page === 1) {
    const users = await (isOrg ? getOrgUsersByIds : getProjectUsersByIds)(userIds, projectId);
    pageUsers.list = unionBy(users.concat(pageUsers.list), 'id');
  }
  return pageUsers;
}
class UserApi extends Api<UserApi> {
  get prefix() {
    return `/iam/choerodon/v1/projects/${this.projectId}`;
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
  getById(userId: string | string) {
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
    number: number
  }> {
    return this.request({
      method: 'get',
      url: `/iam/choerodon/v1/projects/${projectId || getProjectId()}/users`,
      params: {
        param,
        id: userId,
        page: page || 1,
        size: size ?? 20,
      },
      // cache: true,
    });
  }

  async getProjectUsers(param?: string, page?: number, userIds?: string[], queryFilterIds?: string[], size?: number, projectId?: string): Promise<{
    list: User[]
    hasNextPage: boolean
    number: number
  }> {
    return withSelectedUsers(this.request({
      method: 'post',
      url: `/iam/choerodon/v1/projects/${projectId || this.projectId}/users/agile`,
      params: {
        param,
        page: page || 1,
        size: size ?? 20,
      },
      data: queryFilterIds ?? [],
    }), {
      page: page || 1,
      userIds,
      projectId: projectId || this.projectId,
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

  async getOrgUsers(param?: string, page?: number, userIds?: string[], queryFilterIds?: string[], size?: number): Promise<{
    list: User[]
    hasNextPage: boolean
  }> {
    return withSelectedUsers(this.request({
      method: 'post',
      url: `/iam/choerodon/v1/organizations/${this.orgId}/users/agile`,
      params: {
        param,
        page: page || 1,
        size: size || 20,
      },
      data: queryFilterIds ?? [],
    }), {
      isOrg: true,
      page: page || 1,
      userIds,
    });
  }

  async getWorkbenchUsers(params?: { param?: string, page?: number, size?: number }, data: {ignoredUserIds?:string} = {}): Promise<{
    list: User[]
    hasNextPage: boolean
  }> {
    return this.request({
      method: 'post',
      url: `/agile/v1/organizations/${this.orgId}/work_bench/users`,
      params: {
        ...params,
      },
      data,
    });
  }

  /**
   * 在项目层查询用户列表（包括离职用户）
   * @param param
   * @param page
   * @param userId
   */
  async getAllInProjectIncludesLeaveUsers(param?: string, page?: number, userIds?: string[], size?: number) {
    return withSelectedUsers(this.request({
      method: 'post',
      url: `/agile/v1/projects/${this.projectId}/issues/users`,
      params: {
        param,
        page: page || 1,
        size: size || 50,
      },
      data: userIds,
    }), {
      page: page || 1,
      userIds,
      projectId: this.projectId,
    });
  }
}

const userApi = new UserApi();
const userApiConfig = new UserApi(true);

export { userApi, userApiConfig };
