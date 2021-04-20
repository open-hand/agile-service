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
export interface IPublishVersionTreeNode {
  id: string
  name?: string
  version?: string | null
  versionAlias?: string | null
  serviceCode?: string | null
  type?: string | 'app'
  children?: Array<IPublishVersionTreeNode>
}

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
  loadList(page = 0, size = 10, appService = true) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/publish_version/list`,
      data: { appService },
      params: {
        page,
        size,
      },
    });
  }

  loadStory(publishVersionId: string, searchVO: any, params = { page: 1, size: 10 }) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/publish_version/${publishVersionId}/story`,
      data: searchVO,
      params: {
        ...params,
        organizationId: getOrganizationId(),

      },
    });
  }

  loadBug(publishVersionId: string, searchVO: any, params = { page: 1, size: 10 }) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/publish_version/${publishVersionId}/bug`,
      data: searchVO,
      params: {
        ...params,
        organizationId: getOrganizationId(),

      },
    });
  }

  create(data: IPublishVersionCreateData) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/publish_version`,
      data: {
        ...data,
        // @ts-ignore
        appService: typeof (data.appService) === 'undefined' ? true : data.appService,
        organizationId: getOrganizationId(),
        projectId: data.projectId || getProjectId(),
      },
    });
  }

  /**
   * 批量创建
   */
  createBatch(publishVersionId: string, data: IPublishVersionCreateData[]) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/publish_version/${publishVersionId}/batch`,
      data,
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
  importPom(data: any, publishVersionId: string, groupIds?: string) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/publish_version/${publishVersionId}/parse_pom`,
      params: {
        groupIds,
      },
      data,
    });
  }

  /**
   * 加载版本依赖树
   */
  loadDependencyTree(rootId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/publish_version_tree`,
      params: {
        rootId,
        organizationId: getOrganizationId(),
      },
    });
  }

  dependencyTreeAdd(data: IPublishVersionTreeNode) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/publish_version_tree/add`,
      params: {
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  dependencyTreeDel(data: IPublishVersionTreeNode) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/publish_version_tree/delete`,
      params: {
        organizationId: getOrganizationId(),
      },
      data,
    });
  }

  loadDependency(rootId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/publish_version_tree/direct_descendants`,
      params: {
        rootId,
        organizationId: getOrganizationId(),
      },
    });
  }

  loadDependencyTreeAvailableNode(rootId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/publish_version_tree/available_publish_version`,
      params: {
        rootId,
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 检查版本别名是否重复
   * @param alias
   * @returns
   */
  checkAlias(alias: string, publishVersionId?: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/publish_version/checkAlias`,
      params: {
        alias,
        publishVersionId,
        organizationId: getOrganizationId(),
      },
    });
  }

  /**
   * 加载关联的应用版本列表
   */
  loadAppVersionList(versionId: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/product_version/${versionId}/rel_app_version`,
    });
  }

  loadAvailableAppVersionList(versionId: string, serviceCode: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/product_version/${versionId}/un_rel_app_version`,
      params: {
        serviceCode,
        size: 0,
      },
    }).then((res: any) => {
      const newList = res.content.map((i: any) => ({ ...i, name: `${i.artifactId}/${i.versionAlias || i.version}` }));
      const newData = ({ ...res, content: newList, list: newList });
      return newData;
    });
  }

  /**
   * 创建应用版本
   * @param data
   * @returns
   */
  createAppVersion(data: any) {
    return axios({
      method: 'post',
      url: `${this.prefix}/app_version`,
      data: {
        ...data,
        appService: true,
        tag: false,
      },
    });
  }

  /**
   * 应用版本重复校验
   * @param data
   * @returns
   */
  checkAppVersion(data: Pick<any, 'artifactId' | 'serviceCode' | 'version'>) {
    return axios({
      method: 'post',
      url: `${this.prefix}/app_version/check`,
      data,
    });
  }

  /**
   * 删除版本与问题关联关系
   */
  deleteLinkIssueId(issueId: string, versionId: string) {
    return axios({
      method: 'delete',
      url: `${this.prefix}/product_version/${versionId}/issue/${issueId}`,
    });
  }

  importProgramPom(data: any, subProjectId: string, groupIds?: string) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/version_tree/parse_pom`,
      params: {
        groupIds,
        subProjectId,
      },
      data,
    });
  }

  loadProgramAppService(programVersionId: string, subProjectId?: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/version_tree/available_app_version`,
      params: {
        programVersionId,
        subProjectId,
      },
    }).then((res: any) => {
      const newList = res.map((i: any) => ({ ...i, name: `${i.artifactId}/${i.versionAlias || i.version}` }));
      return newList;
    });
  }

  createBranchAndLinkAppService(versionId: string, data: any) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/product_version/${versionId}/create_app_version`,
      data,
    });
  }

  compareTag(versionId: string, data: any[]) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/publish_version/${versionId}/compare`,
      data: data.map((i) => ({ ...i, projectId: i.projectId || getProjectId() })),
    });
  }

  export(publishVersionId: string) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/excel/export_publish_version`,
      params: { publishVersionId },
    });
  }
}

const publishVersionApi = new PublishVersionApi();
const publishVersionApiConfig = new PublishVersionApi(true);
export { publishVersionApi, publishVersionApiConfig };
