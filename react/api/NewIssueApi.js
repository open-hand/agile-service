import { stores, axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

const { AppState } = stores;

export function createIssue(issueObj, applyType = 'agile', projectId = AppState.currentMenuType.id) {
  const issue = {
    projectId,
    ...issueObj,
  };
  return axios.post(`/agile/v1/projects/${projectId}/issues?applyType=${applyType}`, issue);
}

export function createIssueField(issueId, dto) {
  const projectId = AppState.currentMenuType.id;
  const orgId = AppState.currentMenuType.organizationId;
  return axios.post(`/agile/v1/projects/${projectId}/field_value/quick_create/${issueId}?organizationId=${orgId}`, dto);
}

export function loadIssueTypes(applyType) {
  const projectId = AppState.currentMenuType.id;
  return axios.get(`/agile/v1/projects/${projectId}/schemes/query_issue_types_with_sm_id?apply_type=${applyType || 'agile'}`);
}
export function loadLabels() {
  const projectId = AppState.currentMenuType.id;
  return axios.get(
    `/agile/v1/projects/${projectId}/issue_labels`,
  );
}

export function loadVersions(arr = []) {
  const projectId = AppState.currentMenuType.id;
  return axios.post(`/agile/v1/projects/${projectId}/product_version/names`, arr);
}

export function createVersion(versionCreateVO) {
  const projectId = AppState.currentMenuType.id;
  return axios.post(`/agile/v1/projects/${projectId}/product_version`, versionCreateVO);
}
export function checkVersionNameRepeat(value) {
  const projectId = AppState.currentMenuType.id;
  return axios.get(`/agile/v1/projects/${projectId}/product_version/check?name=${value}`);
}
export function getFoundationHeader() {
  const projectId = AppState.currentMenuType.id;
  return axios.get(`/agile/v1/projects/${projectId}/field_value/list/getFields`, {
    params: {
      project_id: projectId * 1,
      organizationId: AppState.currentMenuType.organizationId * 1,
      schemeCode: 'agile_issue',
    },
  });
}
export function createCommit(commitObj, projectId = AppState.currentMenuType.id) {
  return axios.post(`/agile/v1/projects/${projectId}/issue_comment`, commitObj);
}

export function updateCommit(commitObj, projectId = AppState.currentMenuType.id) {
  return axios.post(`/agile/v1/projects/${projectId}/issue_comment/update`, commitObj);
}

export function deleteCommit(commitId, projectId = AppState.currentMenuType.id) {
  return axios.delete(`/agile/v1/projects/${projectId}/issue_comment/${commitId}`);
}

// export function loadComponents() {
//   const projectId = AppState.currentMenuType.id;
//   return axios.get(
//     `/agile/v1/projects/${projectId}/component`,
//   );
// }

export function loadComponents() {
  const projectId = AppState.currentMenuType.id;
  return axios.post(
    `/agile/v1/projects/${projectId}/component/query_all?size=${999}&page=${1}`, {
      advancedSearchArgs: {},
      searchArgs: {},
      content: '',
    },
  );
}

export function loadEpics() {
  const projectId = AppState.currentMenuType.id;
  return axios.get(
    `/agile/v1/projects/${projectId}/issues/epics/select_data`,
  );
}

/**
 * 在项目群中获取史诗列表
 */
export function loadProgramEpics() {
  const projectId = AppState.currentMenuType.id;
  return axios.get(
    `/agile/v1/projects/${projectId}/issues/epics/select_program_data`,
  );
}

/**
 * 根据冲刺状态获取冲刺，["started", "sprint_planning", "closed"]
 * @param {*} arr
 */
export function loadSprints(arr = []) {
  const projectId = AppState.currentMenuType.id;
  return axios.post(`/agile/v1/projects/${projectId}/sprint/names`, arr);
}

export function loadSprint(sprintId = '') {
  const projectId = AppState.currentMenuType.id;
  return axios.get(`/agile/v1/projects/${projectId}/sprint/${sprintId}`);
}

export function loadSprintIssues(sprintId, status, page = 1, size = 99999) {
  const orgId = AppState.currentMenuType.organizationId;
  const projectId = AppState.currentMenuType.id;
  return axios.get(`/agile/v1/projects/${projectId}/sprint/${sprintId}/issues?organizationId=${orgId}&status=${status}&page=${page}&size=${size}`);
}

export function loadChartData(id, type) {
  const projectId = AppState.currentMenuType.id;
  return axios.get(`/agile/v1/projects/${projectId}/reports/${id}/burn_down_report?type=${type}&ordinalType=asc`);
}

export function loadStatus(statusId, issueId, typeId, applyType = 'agile', projectId = AppState.currentMenuType.id) {
  return axios.get(
    `/agile/v1/projects/${projectId}/schemes/query_transforms?current_status_id=${statusId}&issue_id=${issueId}&issue_type_id=${typeId}&apply_type=${applyType}`,
  );
}
export function loadStatusList(applyType = 'agile') {
  const projectId = AppState.currentMenuType.id;
  return axios.get(`/agile/v1/projects/${projectId}/schemes/query_status_by_project_id?apply_type=${applyType}`);
}
// 调用issue服务
export function loadPriorities() {
  const projectId = AppState.currentMenuType.id;
  const orgId = AppState.currentMenuType.organizationId;
  // 进行一层处理，过滤掉禁用的优先级
  return axios.get(`/agile/v1/projects/${projectId}/priority/list_by_org`).then(data => Array.isArray(data) && data.filter(v => v.enable));
}
export function getDefaultPriority() {
  const proId = AppState.currentMenuType.id;
  return axios.get(`/agile/v1/projects/${proId}/priority/default`);
}
export function loadIssue(issueId, projectId = AppState.currentMenuType.id) {
  const orgId = AppState.currentMenuType.organizationId;
  return axios.get(`/agile/v1/projects/${projectId}/issues/${issueId}${orgId ? `?organizationId=${orgId}` : ''}`);
}

export function loadSubtask(issueId, projectId = AppState.currentMenuType.id) {
  return axios.get(`agile/v1/projects/${projectId}/issues/sub_issue/${issueId}`);
}

export function updateIssue(data, projectId = AppState.currentMenuType.id) {
  // if (type === 'sub_task') {
  //   return axios.put(`agile/v1/projects/${projectId}/issues/sub_issue`, data);
  // }
  return axios.put(`/agile/v1/projects/${projectId}/issues`, data);
}

export function updateIssueWSJFDTO(data, projectId = AppState.currentMenuType.id) {
  return axios.post(`/agile/v1/projects/${projectId}/wsjf`, data);
}

export function updateStatus(transformId, issueId, objVerNum, applyType = 'agile', proId = AppState.currentMenuType.id) {
  return axios.put(`/agile/v1/projects/${proId}/issues/update_status?applyType=${applyType}&transformId=${transformId}&issueId=${issueId}&objectVersionNumber=${objVerNum}`);
}

export function createSubIssue(obj, applyType, projectId = AppState.currentMenuType.id) {
  return axios.post(`/agile/v1/projects/${projectId}/issues/sub_issue`, obj);
}

export function deleteIssue(issueId, createBy) {
  if (createBy === AppState.userInfo.id) {
    return axios.delete(`/agile/v1/projects/${getProjectId()}/issues/delete_self_issue/${issueId}`);
  }  
  return axios.delete(`/agile/v1/projects/${getProjectId()}/issues/${issueId}`);
}

export function deleteLink(issueLinkId, projectId = AppState.currentMenuType.id) {
  return axios.delete(`/agile/v1/projects/${projectId}/issue_links/${issueLinkId}`);
}

export function deleteFeatureLink(featureDependId, projectId = AppState.currentMenuType.id) {
  return axios.delete(`/agile/v1/projects/${projectId}/board_depend/${featureDependId}`);
}

export function createWorklog(data, projectId = AppState.currentMenuType.id) {
  return axios.post(`/agile/v1/projects/${projectId}/work_log`, data);
}

export function loadWorklogs(issueId, projectId = AppState.currentMenuType.id) {
  return axios.get(`agile/v1/projects/${projectId}/work_log/issue/${issueId}`);
}

export function loadDatalogs(issueId, projectId = AppState.currentMenuType.id) {
  return axios.get(`agile/v1/projects/${projectId}/data_log?issueId=${issueId}`);
}

export function loadBranchs(issueId, projectId = AppState.currentMenuType.id) {
  return axios.get(`/devops/v1/project/${projectId}/issue/${issueId}/commit_and_merge_request/count`);
}

export function loadDocs(issueId, projectId = AppState.currentMenuType.id) {
  return axios.get(`/agile/v1/projects/${projectId}/knowledge_relation/issue/${issueId}`);
}

export function deleteDoc(id, projectId = AppState.currentMenuType.id) {
  return axios.delete(`/agile/v1/projects/${projectId}/knowledge_relation/${id}`);
}

export function updateWorklog(logId, worklog, projectId = AppState.currentMenuType.id) {
  return axios.patch(`agile/v1/projects/${projectId}/work_log/${logId}`, worklog);
}

export function deleteWorklog(logId, projectId = AppState.currentMenuType.id) {
  return axios.delete(`agile/v1/projects/${projectId}/work_log/${logId}`);
}

export function updateIssueType(data, projectId = AppState.currentMenuType.id) {
  const orgId = AppState.currentMenuType.organizationId;
  const issueUpdateTypeVO = {
    projectId,
    ...data,
  };
  return axios.post(`/agile/v1/projects/${projectId}/issues/update_type?organizationId=${orgId}`, issueUpdateTypeVO);
}

export function transformedTask(data, projectId = AppState.currentMenuType.id) {
  const orgId = AppState.currentMenuType.organizationId;
  const issueUpdateTypeVO = {
    projectId,
    ...data,
  };
  return axios.post(`/agile/v1/projects/${projectId}/issues/transformed_task?organizationId=${orgId}`, issueUpdateTypeVO);
}

export function loadIssues(page = 1, size = 10, searchVO, orderField, orderType) {
  const orgId = AppState.currentMenuType.organizationId;
  const projectId = AppState.currentMenuType.id;
  return axios.post(`/agile/v1/projects/${projectId}/issues/include_sub?page=${page}&size=${size}${orgId ? `&organizationId=${orgId}` : ''}`, searchVO, {
    params: {
      sort: `${orderField && orderType ? `${orderField},${orderType}` : ''}`,
    },
  });
}
/**
 * 关联问题时进行问题查询 (对于BUG管理问题)
 * @param {*} page 
 * @param {*} size 
 * @param {*} searchVO  
 */
export function loadLinkIssuesForBug(page = 1, size = 10, searchVO) {
  const orgId = AppState.currentMenuType.organizationId;
  const projectId = AppState.currentMenuType.id;
  return axios.post(`/agile/v1/projects/${projectId}/issues/query_story_task?page=${page}&size=${size}`, searchVO);
}

export function loadIssuesInLink(page = 1, size = 10, issueId, content) {
  const projectId = AppState.currentMenuType.id;
  if (issueId && content) {
    return axios.get(`/agile/v1/projects/${projectId}/issues/agile/summary?issueId=${issueId}&self=false&content=${content}&page=${page}&size=${size}`);
  } else if (issueId && !content) {
    return axios.get(`/agile/v1/projects/${projectId}/issues/agile/summary?issueId=${issueId}&self=false&page=${page}&size=${size}`);
  } else if (!issueId && content) {
    return axios.get(`/agile/v1/projects/${projectId}/issues/agile/summary?self=false&content=${content}&page=${page}&size=${size}`);
  } else {
    return axios.get(`/agile/v1/projects/${projectId}/issues/agile/summary?self=false&page=${page}&size=${size}`);
  }
}

export function loadFeaturesInLink(page = 1, size = 10, issueId, content) {
  const projectId = AppState.currentMenuType.id;
  if (issueId && content) {
    return axios.get(`/agile/v1/projects/${projectId}/issues/agile/feature?issueId=${issueId}&self=false&content=${content}&page=${page}&size=${size}`);
  } else if (issueId && !content) {
    return axios.get(`/agile/v1/projects/${projectId}/issues/agile/feature?issueId=${issueId}&self=false&page=${page}&size=${size}`);
  } else if (!issueId && content) {
    return axios.get(`/agile/v1/projects/${projectId}/issues/agile/feature?self=false&content=${content}&page=${page}&size=${size}`);
  } else {
    return axios.get(`/agile/v1/projects/${projectId}/issues/agile/feature?self=false&page=${page}&size=${size}`);
  }
}

export function createLink(issueId, issueLinkCreateVOList) {
  const projectId = AppState.currentMenuType.id;
  return axios.post(`/agile/v1/projects/${projectId}/issue_links/${issueId}`, issueLinkCreateVOList);
}

export function createFeatureLink(data) {
  const projectId = AppState.currentMenuType.id;
  return axios.post(`/agile/v1/projects/${projectId}/board_depend/batch_create_depend`, data);
}

export function loadLinkIssues(issueId, applyType = 'project') {
  const projectId = AppState.currentMenuType.id;
  // eslint-disable-next-line no-cond-assign
  if (applyType === 'project') {
    return axios.get(`/agile/v1/projects/${projectId}/issue_links/${issueId}`);
  } else {
    return axios.get(`/agile/v1/projects/${projectId}/board_depend/feature_depend/${issueId}`);
  }
}

/**
 *下载导入模板
 *
 * @export
 * @returns
 */
export function exportExcelTmpl() {
  const projectId = AppState.currentMenuType.id;
  const orgId = AppState.currentMenuType.organizationId;
  return axios.get(`/agile/v1/projects/${projectId}/excel/download?organizationId=${orgId}`, { responseType: 'arraybuffer' });
}

/**
 * 导入issue
 * @param data
 * @returns {*}
 */
export function importIssue(data) {
  const axiosConfig = {
    headers: { 'content-type': 'multipart/form-data' },
  };
  const projectId = AppState.currentMenuType.id;
  const orgId = AppState.currentMenuType.organizationId;
  const userId = AppState.getUserId;
  return axios.post(`/agile/v1/projects/${projectId}/excel/import?organizationId=${orgId}&userId=${userId}`, data, axiosConfig);
}

/**
 * 查询最新的导入记录
 * @returns {V|*}
 */
export function queryImportHistory() {
  const projectId = AppState.currentMenuType.id;
  return axios.get(`/agile/v1/projects/${projectId}/excel/latest`);
}

/**
 * 取消导入
 * @returns {V|*}
 */
export function cancelImport(id, ovn) {
  const projectId = AppState.currentMenuType.id;
  return axios.put(`/agile/v1/projects/${projectId}/excel/cancel?id=${id}&objectVersionNumber=${ovn}`);
}

/**
 * 加载字段配置
 * @returns {V|*}
 */
export function getFields(dto) {
  const projectId = AppState.currentMenuType.id;
  const orgId = AppState.currentMenuType.organizationId;
  return axios.post(`/agile/v1/projects/${projectId}/field_value/list?organizationId=${orgId}`, dto);
}

/**
 * 加载字段配置（包含值）
 * @returns {V|*}
 */
export function getFieldAndValue(id, dto) {
  const projectId = AppState.currentMenuType.id;
  const orgId = AppState.currentMenuType.organizationId;
  return axios.post(`/agile/v1/projects/${projectId}/field_value/list/${id}?organizationId=${orgId}`, dto);
}

/**
 * 更新Issue字段值
 * @returns {V|*}
 */
export function updateFieldValue(id, fieldId, code, dto) {
  const projectId = AppState.currentMenuType.id;
  const orgId = AppState.currentMenuType.organizationId;
  return axios.post(`/agile/v1/projects/${projectId}/field_value/update/${id}?organizationId=${orgId}&fieldId=${fieldId}&schemeCode=${code}`, dto);
}

/**
 * 新增Issue字段值
 * @returns {V|*}
 */
export function createFieldValue(id, code, dto) {
  const projectId = AppState.currentMenuType.id;
  const orgId = AppState.currentMenuType.organizationId;
  return axios.post(`/agile/v1/projects/${projectId}/field_value/${id}?organizationId=${orgId}&schemeCode=${code}`, dto);
}
export function getMyFilters() {
  const { userInfo: { id } } = AppState;
  const projectId = AppState.currentMenuType.id;
  return axios.get(`/agile/v1/projects/${projectId}/personal_filter/query_all/${id}`);
}

export function createMyFilter(data) {
  const projectId = AppState.currentMenuType.id;
  return axios.post(`/agile/v1/projects/${projectId}/personal_filter`, data);
}
export function checkMyFilterName(filterName) {
  const projectId = AppState.currentMenuType.id;
  return axios.get(`/agile/v1/projects/${projectId}/personal_filter/check_name?userId=${AppState.userInfo.id}&name=${filterName}`);
}
export function updateMyFilter(filterId, updateData) {
  const projectId = AppState.currentMenuType.id;
  return axios.put(`/agile/v1/projects/${projectId}/personal_filter/${filterId}`, updateData);
}
export function deleteMyFilter(filterId) {
  const projectId = AppState.currentMenuType.id;
  return axios.delete(`/agile/v1/projects/${projectId}/personal_filter/${filterId}`);
}
export function getTestExecute(issueId) {
  const projectId = AppState.currentMenuType.id;
  return axios.get(`/test/v1/projects/${projectId}/defect/query_by_bug?bugId=${issueId}`);
}

export function loadSprintsByTeam(teamId, piId) {
  const projectId = AppState.currentMenuType.id;
  return axios.get(`/agile/v1/projects/${projectId}/sprint/sub_project/${teamId}/list_by_team_id?piId=${piId}`);
}
export function getHistoryPI(issueId) {
  const projectId = AppState.currentMenuType.id;
  return axios.get(`/agile/v1/projects/${projectId}/pi/${issueId}/list_feature_pi_log`);
}
export function getCustomFields() {
  const projectId = AppState.currentMenuType.id;
  return axios.get(`/agile/v1/projects/${projectId}/field_value/list/custom_field`);
}
export function batchUpdateIssue(data) {
  const projectId = AppState.currentMenuType.id;
  return axios.post(`/agile/v1/projects/${projectId}/field_value/batch_update_fields_value?schemeCode=agile_issue`, data);
}
