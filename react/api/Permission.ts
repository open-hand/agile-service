import { axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '@/utils/common';

class PermissionApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  /**
   *根据permissionCodes进行本项目权限检查
   * @param permissionCodes 
   */
  check(permissionCodes: Array<string>) {
    const projectId = getProjectId();
    return axios({
      method: 'post',
      url: '/iam/choerodon/v1/permissions/menus/check-permissions',
      params: {
        projectId,
        tenantId: getOrganizationId()
      },
      data: permissionCodes,
    });
  }
}

const permissionApi = new PermissionApi();
export { permissionApi };
