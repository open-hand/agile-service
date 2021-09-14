import { axios, stores, Choerodon } from '@choerodon/boot';
import { getProjectId, getOrganizationId, getApplyType } from '@/utils/common';
import { sameProject } from '@/utils/detail';
import Api from './Api';

const { AppState } = stores;
interface IIssue {
  summary: string, // 概要
  epicName?: string, // 史诗名称
  typeCode: string, // 问题类型
  priorityId: number, // 优先级id
  priorityCode: string, // 优先级code "priority-17"
  projectId: number, // 项目id
  programId?: number, // 项目群id
  assigneeId?: number, // 经办人id
  componentIssueRelVOList?: Array<any>, // 模块信息 [{name: "敏捷", projectId: 1528}]
  description?: string, // 描述
  epicId?: number, // 史诗id
  featureVO?: object, // 特性 {}
  issueLinkCreateVOList?: Array<object>, // 关联的问题列表 [{linkTypeId: "4833", linkedIssueId: 275652, in: false}]//问题链接
  issueTypeId?: number, // 问题类型id
  labelIssueRelVOList?: Array<any>, // 标签列表 []
  parentIssueId?: number, // 父id 为0代表自己是父问题
  piId?: number, // pi Id
  relateIssueId?: number, // 关联的问题id
  sprintId?: number, // 冲刺id
  storyPoints?: number | string, // 故事点
  versionIssueRelVOList?: Array<object>, // 关联的版本信息 [{versionId: 1814, relationType: "fix"}]
  wsjfVO?: object, // wsjf信息 {}
  // [ propName : string ] : any,//
}
interface UIssue {
  issueId: string,
  objectVersionNumber: number,
  [propName: string]: any,
}
interface UTypeAndStatus {
  issueId: string,
  issueTypeId: string, // 问题类型id
  objectVersionNumber: number,
  projectId?: string,
  typeCode: string,
  statusId?: string, // 状态id
  parentIssueId?: string// 父id
  epicName?: string
  batchUpdateFieldsValueVo?: {
    issueIds: string[],
    predefinedFields: object
    customFields: {
      fieldId: string,
      fieldType: string,
      value: any,
    }[]
  }
}
interface UIssueParent {
  issueId: string,
  parentIssueId: string,
  objectVersionNumber: number
}
interface SearchVO {
  advancedSearchArgs: object,
}
interface CopyCondition {
  issueLink: boolean,
  subTask: boolean,
  summary: string,
  epicName?: string,
  predefinedFieldNames: string[],
  customFieldIds: string[],
}

interface ICustomFieldData {
  option: Array<{ fieldId: string, value: any }>,
  date: Array<any>,
  // eslint-disable-next-line camelcase
  date_hms: Array<any>,
  number: Array<any>,
  string: Array<any>,
  text: Array<any>,
}
interface IExportSearch {
  advancedSearchArgs?: {
    issueTypeId?: number, // 问题类型id
    reporterIds?: Array<number>, // 报告人id列表
    statusId?: number, // 状态id
    priorityId?: number, // 优先级id
  },
  otherArgs: {
    customField?: ICustomFieldData, // 通用组件 （自定义）
    assigneeId?: number, // 经办人id
    issueIds?: Array<number>,
    component?: any,
    epic?: any,
    feature?: any,
    label?: any,
    sprint?: any,
    summary?: string,
    fixVersion?: any,
    influenceVersion?: any,
    starBeacon?: boolean
    myAssigned?: boolean
    userId?: string
    testResponsibleIds?: string[]
    mainResponsibleIds?: string[]
    environment?: string[]
    creatorIds?: string
    updatorIds?: string[]
    tags?: any[]
  },
  searchArgs?: {
    estimatedStartTimeScopeStart?: string,
    estimatedStartTimeScopeEnd?: string,
    estimatedEndTimeScopeStart?: string,
    estimatedEndTimeScopeEnd?: string,
    createStartDate: string,
    createEndDate: string,
    updateStartDate: string,
    updateEndDate: string,
  },
  exportFieldCodes: Array<string>, // 导出的字段列表
  quickFilterIds?: Array<number>,
  contents?: string | string[],
}
interface IImportOrExportRecord {
  action: null | string
  creationDate: null | string
  failCount: null | number
  fileUrl: null | string
  id: null | string
  lastUpdateDate: null | string
  objectVersionNumber: null | number
  organizationId: null | string
  projectId: null | string
  status: null | string
  successCount: null | number
  userId: null | string
}
export { IExportSearch, ICustomFieldData, IImportOrExportRecord };
class IssueApi extends Api<IssueApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  get outPrefix() {
    return '/agile/v1/backlog_external';
  }

  get isOutside() {
    return false;
  }

  outside(outside: boolean) {
    return this.overwrite('isOutside', outside);
  }

  /**
    * 创建问题 敏捷/测试
    * @param issueObj
    * @param applyType
    */
  create = (issueObj: IIssue, applyType: string = 'agile') => axios({
    method: 'post',
    url: `${this.prefix}/issues`,
    data: issueObj,
    params: {
      applyType,
    },
  });

  /**
    * 更新问题
    * @param issueObj
    * @param projectId
    */
  update = async (issueObj: UIssue) => {
    try {
      return await axios.put(`${this.prefix}/issues`, issueObj, {
        noPrompt: true,
      });
    } catch (error) {
      if (error.code === 'error.dataLogEpic.methodExecute') {
        Choerodon.prompt('该问题项详情信息已被锁定，请重新打开问题详情进行编辑。', 'error');
      } else {
        Choerodon.prompt(error.message);
      }
      throw error;
    }
  }

  /**
    * 更新问题状态
    * @param transformId 转换的状态id
    * @param issueId  问题id
    * @param objectVersionNumber 版本号
    * @param applyType 应用类型
    */
  updateStatus(transformId: number, issueId: number, objectVersionNumber: number, applyType = 'agile') {
    return axios({
      method: 'put',
      url: `${this.prefix}/issues/update_status`,
      params: {
        issueId,
        applyType,
        transformId,
        objectVersionNumber,
      },
    }).then((res: any) => {
      if (typeof (res) === 'object') {
        const { errorMsg } = res;
        errorMsg && Choerodon.prompt(errorMsg, 'error');
      }
      return res;
    });
  }

  getRequiredField(issueId: string, issueTypeId: string) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'get',
      url: `${this.prefix}/issues/${issueId}/list_required_field`,
      params: {
        organizationId,
        issueTypeId,
      },
    });
  }

  /**
    * 更新问题类型
    * @param data
    */
  updateType(data: UTypeAndStatus) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'post',
      url: `${this.prefix}/issues/update_type`,
      params: {
        organizationId,
      },
      data,
    });
    // return axios.post(`/agile/v1/projects/${projectId}/issues/update_type?organizationId=${orgId}`, issueUpdateTypeVO);
  }

  /**
    * 克隆问题
    * @param issueId
    * @param applyType
    * @param copyCondition
    */
  clone(issueId: string, applyType: string = 'agile', copyCondition: CopyCondition) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'post',
      url: `${this.prefix}/issues/${issueId}/clone_issue`,
      data: copyCondition,
      params: {
        organizationId,
        applyType,
      },
    });
  }

  /**
    * 根据问题id加载问题
    * @param issueId
    */
  load(issueId: string, projectId?: string) {
    return this.isOutside ? this.request({
      method: 'get',
      url: `${this.outPrefix}/issues/${issueId}`,
      params: {
        project_id: this.projectId,
        organizationId: this.orgId,
      },
    }) : this.request({
      method: 'get',
      url: `${`/agile/v1/projects/${projectId || getProjectId()}`}/${sameProject(this.projectId) ? '' : 'project_invoke_agile/'}issues/${issueId}`,
      params: {
        organizationId: this.orgId,
        instanceProjectId: this.projectId,
      },
    });
  }

  /**
   * 项目层中加载问题（项目群）
   * @param issueId 问题id
   * @param programId 项目群id
   */
  loadUnderProgram(issueId: number, programId: number) {
    const organizationId = getOrganizationId();
    return this.request({
      method: 'get',
      url: `${this.prefix}/project_invoke_program/issue/${issueId}`,
      params: {
        programId,
        organizationId,
      },
    });
  }

  /**
    * 删除问题
    * @param issueId//问题id
    * @param creatorId//问题创建者id
    */
  delete(issueId: number, creatorId: string) {
    if (creatorId.toString() === AppState.userInfo.id.toString()) {
      return axios.delete(`/agile/v1/projects/${getProjectId()}/issues/delete_self_issue/${issueId}`);
    }
    return axios.delete(`/agile/v1/projects/${getProjectId()}/issues/${issueId}`);
  }

  /**
    * 导出问题列表
    * @param searchVO
    * @param sort
    */
  export(searchVO: IExportSearch, sort?: string) {
    const organizationId = getOrganizationId();
    return axios({
      url: `${this.prefix}/excel/export`,
      method: 'post',
      data: searchVO,
      params: {
        organizationId,
        sort,
      },
      // responseType: 'arraybuffer',
    });
  }

  /**
 * 导入issue
 * @param data
 * @returns {*}
 */
  import(data: any) {
    // const headers = {
    //   'content-type': 'multipart/form-data',
    // };
    const organizationId = getOrganizationId();
    const userId = AppState.getUserId;
    return axios({
      headers: { 'Content-Type': 'multipart/form-data' },
      method: 'post',
      url: getApplyType() === 'program' ? `${this.prefix}/issues/import` : `${this.prefix}/excel/import`,
      params: {
        organizationId,
        userId,
      },
      data,
    });
  }

  /**
 * 取消导入
 * @param id 导入id
 */
  cancelImport(id: number | string, objectVersionNumber: number) {
    return axios({
      method: 'put',
      url: `${this.prefix}/excel/cancel`,
      params: {
        id,
        objectVersionNumber,
      },
    });
  }

  /**
 * 查询最新的导入导出记录
 * @returns {V|*}
 */
  loadLastImportOrExport(action: 'upload_file' | 'download_file' | 'upload_file_customer_field' | 'upload_file_backlog' | 'download_file_publish_version'): Promise<IImportOrExportRecord> {
    return axios({
      url: `${this.prefix}/excel/latest`,
      method: 'get',
      params: {
        action,
      },
    });
  }

  /**
 *下载导入模板
 *
 * @export
 * @returns
 */
  downloadTemplateForImport(data: { systemFields: string[], customFields: string[] }) {
    const organizationId = getOrganizationId();
    if (getApplyType() === 'program') {
      return axios({
        method: 'post',
        url: `${this.prefix}/issues/template`,
        responseType: 'arraybuffer',
        data,
      });
    }
    return axios({
      method: 'post',
      url: `${this.prefix}/excel/download`,
      params: {
        organizationId,
      },
      data,
      responseType: 'arraybuffer',
    });
  }

  /**
    * 创建子任务
    * @param obj
    * @param applyType
    */
  createSubtask = (issueObj: object) => axios.post(`${this.prefix}/issues/sub_issue`, issueObj)

  /**
    * 根据子任务问题id 进行加载这个子任务（废弃，不再使用）
    * @param issueId
    */
  loadSubtask(issueId: number) {
    return axios.get(`${this.prefix}/issues/sub_issue/${issueId}`);
  }

  /**
    * 子任务转换为任务
    * @param data
    */
  subtaskTransformTask(data: UTypeAndStatus) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'post',
      url: `${this.prefix}/issues/transformed_task`,
      data,
      params: {
        organizationId,
      },
    });
  }

  /**
    * 任务转换为子任务
    * @param data
    */
  taskTransformSubTask(data: UTypeAndStatus) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'post',
      url: `${this.prefix}/issues/transformed_sub_task`,
      data,
      params: {
        organizationId,
      },
    });
  }

  /**
    * 更改子任务所属的父任务
    * @param issueUpdateParentIdVO
    */
  subTaskChangeParent(issueUpdateParentIdVO: UIssueParent) {
    return axios.post(`${this.prefix}/issues/update_parent`, issueUpdateParentIdVO);
  }

  /**
  * 查询故事和任务   关联问题时 (对于BUG管理问题)
  * @param {*} page
  * @param {*} size
  * @param {*} searchVO
  */
  loadStroyAndTask(page: number = 1, size: number = 10, searchVO?: SearchVO) {
    return axios({
      method: 'post',
      url: `${this.prefix}/issues/query_story_task`,
      params: {
        page,
        size,
      },
      data: searchVO,
    });
  }

  /**
    * 分页搜索查询issue列表
    * @param page
    * @param size
    * @param issueId
    * @param content
    */
  loadIssuesInLink(page: number = 1, size: number = 10, issueId?: string, content?: string, excludeIssueIds?: string[]) {
    return axios({
      method: 'post',
      url: `/agile/v1/projects/${this.projectId}/issues/agile/summary`,
      params: {
        page,
        size,
        issueId,
        content,
        self: false,
      },
      data: excludeIssueIds,
    });
  }

  encryptIssueId(issueId: number) {
    return axios({
      method: 'get',
      url: `/agile/v1/projects/${getProjectId()}/encrypt`,
      params: {
        issueId,
      },
    });
  }

  loadParentIssues(page: number, size: number = 20, issueType: 'bug' | 'sub_task', param?: string) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/issues/available_parents`,
      params: {
        issueType,
        param,
        page,
        size,
      },
    });
  }

  star(issueId: string) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/star_beacon/instance/${issueId}/star`,
      data: {
        type: 'issue',
        organizationId: getOrganizationId(),
      },
    });
  }

  unstar(issueId: string) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/star_beacon/instance/${issueId}/unstar`,
      data: {
        type: 'issue',
        organizationId: getOrganizationId(),
      },
    });
  }

  batchDelete(issueIds: string[]) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/issues/batch_delete`,
      data: issueIds,
    });
  }

  loadIssues(page: number, size: number, sort: string | undefined, search: SearchVO) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/issues/include_sub`,
      params: {
        page,
        size,
        sort,
      },
      data: search,
    });
  }

  decrypt(issueId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/decrypt`,
      params: {
        issueId,
      },
    });
  }

  getUnLinkedIssues(issueId: string, data: any) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/issue_links/un_link/${issueId}`,
      data,
    });
  }

  checkRequiredFields(issueTypeId: string, data: string[]) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/field_value/filter_require_field`,
      params: {
        issueTypeId,
      },
      data,
    });
  }

  getComments(issueId: string, page = 1, size = 10) {
    return this.isOutside ? this.request({
      method: 'get',
      url: `${this.outPrefix}/issue_comment/issue/${issueId}/page`,
      params: {
        project_id: this.projectId,
        organizationId: this.orgId,
        page,
        size,
      },
    }) : this.request({
      method: 'get',
      url: `${`/agile/v1/projects/${getProjectId()}`}/${sameProject(this.projectId) ? '' : 'project_invoke_agile/'}issue_comment/issue/${issueId}/page`,
      params: {
        organizationId: this.orgId,
        instanceProjectId: this.projectId,
        page,
        size,
      },
    });
  }

  getCommentsUnderProgram(issueId: number, programId: number, page = 1, size = 10) {
    const organizationId = getOrganizationId();
    return this.request({
      method: 'get',
      url: `${this.prefix}/project_invoke_program/issue_comment/issue/${issueId}/page`,
      params: {
        programId,
        organizationId,
        page,
        size,
      },
    });
  }
}
const issueApi = new IssueApi();
const issueApiConfig = new IssueApi(true);
export { issueApi, issueApiConfig };
