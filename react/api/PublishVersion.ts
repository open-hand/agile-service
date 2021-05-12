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
  appService: boolean | true
  artifactId: string | null
  children: string | null
  content: string | null
  groupId: string | null
  id: string
  objectVersionNumber: number
  organizationId: string
  projectId: string
  serviceCode: string | null
  tagName: string | null
  version: string | null
  versionAlias: string | null
  statusCode: 'version_planning' | 'released' | 'archived'
  description: string | null
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
  type?: string | 'app' | 'publish' | 'tag'
  children?: Array<IPublishVersionTreeNode>
  appServiceCode?: string | null
  artifactId?: string | null
  groupId?: string | null
  projectId?: string | null
  tagName?: string | null
}
interface IPublishVersionListSearchData {
  appService?: boolean
  content?: string
}
interface IPublishVersionTreeTagNode extends Pick<IPublishVersionTreeNode, 'versionAlias'> {
  tagName: string
  appServiceCode: string
  projectId: string
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
  loadList(params = { page: 0, size: 10 }, data: IPublishVersionListSearchData = { appService: true, content: '' }) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/publish_version/list`,
      data,
      params,
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

  loadIssues(publishVersionId: string, searchVO: any, params = { page: 1, size: 10 }) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/publish_version/${publishVersionId}/issues`,
      data: searchVO,
      params: {
        ...params,
        organizationId: getOrganizationId(),

      },
    });
  }

  /** 版本对比处加载应用服务列表 */
  loadAppServiceList(publishVersionId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/publish_version/${publishVersionId}/active_app_service`,
      params: {
        organizationId: getOrganizationId(),

      },
    });
  }

  /** 版本信息查看处加载问题类型列表 */
  loadIssueTypeList(publishVersionId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/publish_version/${publishVersionId}/issue_detail/issue_type`,
      params: {
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

  update(publishVersionId: string, data: any, statusCode?: 'released' | 'version_planning') {
    return this.request({
      method: 'put',
      url: `${this.prefix}/publish_version/update/${publishVersionId}`,
      params: {
        statusCode,
      },
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

  dependencyTreeAddTag(publishVersionId: string, data: IPublishVersionTreeTagNode[]) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/publish_version_tree/add_tag`,
      params: {
        publishVersionId,
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

  dependencyTreeDelTag(publishVersionId: string, data: IPublishVersionTreeTagNode[]) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/publish_version_tree/delete_tag`,
      params: {
        publishVersionId,
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
      url: `${this.prefix}/program_version_tree/parse_pom`,
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
      url: `${this.prefix}/program_version_tree/available_app_version`,
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

  loadCompareHistory(versionId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/publish_version/${versionId}/tag_compare_history`,
      params: {
        organizationId: getOrganizationId(),
      },
    });
  }

  compareTag(versionId: string, data: any[], action?: 'add'|'update') {
    return this.request({
      method: 'post',
      url: `${this.prefix}/publish_version/${versionId}/compare`,
      params: {
        action,
        organizationId: getOrganizationId(),
      },
      data: data.map((i) => ({ ...i, projectId: i.projectId || getProjectId() })),
    });
  }

  comparePreviewTag(versionId: string, data: any[]) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/publish_version/${versionId}/compare/preview_issue`,
      params: {
        organizationId: getOrganizationId(),
      },
      data: {
        advancedSearchArgs: {},
        otherArgs: {},
        searchArgs: { tagCompareList: data.map((i) => ({ ...i, projectId: i.projectId || getProjectId() })) },
      },
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
