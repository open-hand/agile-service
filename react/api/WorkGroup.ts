import { forEach } from 'lodash';
import Api from './Api';

interface CreateFormDataProps {
  name: string,
  parentId: string | number,
}

export interface EditFormDataProps {
  id: string,
  name: string,
  objectVersionNumber: number,
}

interface MoveFormDataPros {
  workGroupId: string,
  before: boolean,
  outSetId: string,
}

class WorkGroupApi extends Api<WorkGroupApi> {
  get prefix() {
    return `/agile/v1/organizations/${this.orgId}/work_group`;
  }

  /**
   * 查询工作组树形数据
   */
  loadWorkGroup() {
    return this.request({
      method: 'get',
      url: `${this.prefix}/query_tree`,
    });
  }

  /**
   * 创建工作组
   * @param data
   */
  createWorkGroup(data: CreateFormDataProps) {
    return this.request({
      method: 'post',
      url: `${this.prefix}`,
      data,
    });
  }

  /**
   * 修改工作组
   * @param data
   */
  editWorkGroup(data: EditFormDataProps) {
    return this.request({
      method: 'put',
      url: `${this.prefix}`,
      data,
    });
  }

  /**
   * 删除工作组
   * @param groupId
   */
  deleteWorkGroup(groupId: string) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/${groupId}`,
    });
  }

  /**
   * 移动工作组
   * @param data
   * @param parentId
   */
  moveWorkGroup(data: MoveFormDataPros, parentId: string | 0) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/move`,
      params: { parentId },
      data,
    });
  }

  /**
   * 查询工作组下成员
   * @param data
   */
  loadUserByGroup(data: object, params?: { page?: number, size?: number }) {
    return this.request({
      method: 'post',
      url: `${this.prefix}_user_rel/page`,
      data,
      params: params && ({
        page: params.page || 1,
        size: params.size || 50,
      }),
    });
  }

  /**
   * 查询未分配工作组成员
   * @param data
   */
  loadUserUnAssignee(data: object) {
    return this.request({
      method: 'post',
      url: `${this.prefix}_user_rel/page_unassignee`,
      data,
    });
  }

  /**
   * 查询未分配某个工作组的成员（如果没有传workGroupId则为查询组织下所有成员）
   * @param data
   */
  loadUserUnAssigneeByGroup(data: object) {
    return this.request({
      method: 'post',
      url: `${this.prefix}_user_rel/unlink`,
      data,
    });
  }

  /**
   * 添加工作组成员
   * @param workGroupId
   * @param userIds
   */
  addUserByGroup(workGroupId: string, userIds: string[]) {
    return this.request({
      method: 'post',
      url: `${this.prefix}_user_rel/batch_insert`,
      data: {
        workGroupId,
        userIds,
      },
    });
  }

  /**
   * 移除工作组成员
   * @param workGroupId
   * @param userIds
   */
  removeUserByGroup(workGroupId: string, userIds: string[]) {
    return this.request({
      method: 'post',
      url: `${this.prefix}_user_rel/batch_delete`,
      data: {
        workGroupId,
        userIds,
      },
    });
  }
}

const workGroupApi = new WorkGroupApi();
const workGroupApiConfigApi = new WorkGroupApi(true);
export { workGroupApi, workGroupApiConfigApi };
