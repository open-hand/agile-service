import { axios, stores } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '@/utils/common';

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
  issueLinkCreateVOList?: Array<object>, // 关联的问题列表 [{linkTypeId: "4833", linkedIssueId: 275652, in: false}] // 问题链接
  issueTypeId?: number, // 问题类型id
  labelIssueRelVOList?: Array<any>, // 标签列表 []
  parentIssueId?: number, // 父id 为0代表自己是父问题
  piId?: number, // pi Id
  relateIssueId?: number, // 关联的问题id
  sprintId?: number, // 冲刺id
  storyPoints?: number | string, // 故事点
  versionIssueRelVOList?: Array<object>, // 关联的版本信息 [{versionId: 1814, relationType: "fix"}] 
  wsjfVO?: object, // wsjf信息 {}
  // [ propName : string ] : any, // 
}
interface UIssue {
  issueId: number,
  objectVersionNumber: number,
  [propName: string]: any,
}
interface UTypeAndStatus {
  issueId: number,
  issueTypeId: number, // 问题类型id
  objectVersionNumber: number,
  projectId: number | string,
  typeCode: string,
  statusId?: number, // 状态id
  parentIssueId?:number // 父id 
}
interface SearchVO {
  advancedSearchArgs: object,
}
class IssueApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
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
  update = (issueObj: UIssue) => axios.put(`${this.prefix}/issues`, issueObj)

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
    });
  }

  /**
   * 更新问题类型
   * @param data 
   */
  updateType(data: UTypeAndStatus) {
    const issueUpdateTypeVO = {
      ...data,
    };
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
   * 根据问题id加载问题
   * @param issueId 
   */
  load(issueId: number) {
    const organizationId = getOrganizationId();
    return axios({
      method: 'get',
      url: `${this.prefix}/issues/${issueId}`,
      params: {
        organizationId,
      },
    });
  }

  /**
   * 删除问题
   * @param issueId // 问题id
   * @param creatorId //问题创建者id
   */
  delete(issueId: number, creatorId: number) {
    if (creatorId === AppState.userInfo.id) {
      return axios.delete(`/agile/v1/projects/${getProjectId()}/issues/delete_self_issue/${issueId}`);
    }
    return axios.delete(`/agile/v1/projects/${getProjectId()}/issues/${issueId}`);
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
  taskTransformSubTask(data:UTypeAndStatus) {
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
 * 查询故事和任务   关联问题时 (对于BUG管理问题)
 * @param {*} page 
 * @param {*} size 
 * @param {*} searchVO  
 */
  loadStroyAndTask(page: number = 1, size: number = 10, searchVO: SearchVO) {
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
  loadIssuesInLink(page: number = 1, size: number = 10, issueId: number, content: string) {
    // console.log('loadIssuesInLink', issueId, content);
    return axios({
      method: 'get',
      url: `${this.prefix}/issues/agile/summary`,
      params: {
        page,
        size,
        issueId,
        content,
        self: false,
      },
    });
    // if (issueId && content) {
    //   return axios.get(`${this.prefix}/issues/agile/summary?issueId=${issueId}&self=false&content=${content}&page=${page}&size=${size}`);
    // } else if (issueId && !content) {
    //   return axios.get(`${this.prefix}/issues/agile/summary?issueId=${issueId}&self=false&page=${page}&size=${size}`);
    // } else if (!issueId && content) {
    //   return axios.get(`${this.prefix}/issues/agile/summary?self=false&content=${content}&page=${page}&size=${size}`);
    // } else {
    //   return axios.get(`${this.prefix}/issues/agile/summary?self=false&page=${page}&size=${size}`);
    // }
  }
}
const issueApi = new IssueApi();
// eslint-disable-next-line import/prefer-default-export
export { issueApi };
