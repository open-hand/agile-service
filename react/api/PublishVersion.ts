import { axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '@/utils/common';
import Api from './Api';

interface IPublishVersionCreateData {
  version: string
  groupId?: string
  artifactId?: string
  versionAlias?: string
  serviceCode?: string
  actualPublishDate?: string
  projectId?: string
  organizationId?: string
}
export interface IPublishVersionData {
  actualPublishDate: string | null
  appService: string | true
  artifactId: string | null
  children: string | null
  content: string | null
  groupId: string | null
  id: string
  objectVersionNumber: number
  organizationId: string
  projectId: string
  serviceCode: string | null
  tagId: string | null
  version: string | null
  versionAlias: string | null
  [propsName: string]: any
}
// export interface IAppVersionData {
//   artifactId: string
//   groupId: string
//   id: null | string
//   organizationId: string
//   projectId: string
//   serviceCode: string
//   version: null | string
//   versionAlias: null | string
// }

class PublishVersionApi extends Api<PublishVersionApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  load(publishVersionId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/publish_version/${publishVersionId}`,
    });
  }

  /**
   *加载发布版本列表
   */
  loadList() {
    return this.request({
      method: 'post',
      url: `${this.prefix}/publish_version/list`,
    });
  }

  create(data: IPublishVersionCreateData) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/publish_version`,
      data: {
        ...data,
        organizationId: getOrganizationId(),
        projectId: data.projectId || getProjectId(),
      },
    });
  }

  /**
   * 批量创建
   */
  createBatch(data: IPublishVersionCreateData) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/publish_version/batch`,
    });
  }

  update(publishVersionId: string, data: any) {
    return this.request({
      method: 'put',
      url: `${this.prefix}/publish_version/update/${publishVersionId}`,
      data: {
        ...data,
        organizationId: getOrganizationId(),
        projectId: data.projectId || getProjectId(),
      },
    });
  }

  delete(publishVersionId: string) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/publish_version/delete/${publishVersionId}`,
    });
  }

  check(data: any) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/publish_version/existed`,
      data,
    });
  }

  /**
   * 导入pom文件解析
   * @param publishVersionId
   * @param data
   * @returns
   */
  importPom(publishVersionId: string, data: any) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/publish_version/${publishVersionId}/parse_pom`,
      data,
    });
  }
}

const publishVersionApi = new PublishVersionApi();
const publishVersionApiConfig = new PublishVersionApi(true);
export { publishVersionApi, publishVersionApiConfig };
