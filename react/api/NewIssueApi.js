import { stores, axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

const { AppState } = stores;


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


export function updateIssueWSJFDTO(data, projectId = AppState.currentMenuType.id) {
  return axios.post(`/agile/v1/projects/${projectId}/wsjf`, data);
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

export function getHistoryPI(issueId) {
  const projectId = AppState.currentMenuType.id;
  return axios.get(`/agile/v1/projects/${projectId}/pi/${issueId}/list_feature_pi_log`);
}

export function batchUpdateIssue(data) {
  const projectId = AppState.currentMenuType.id;
  return axios.post(`/agile/v1/projects/${projectId}/field_value/batch_update_fields_value?schemeCode=agile_issue&applyType=agile`, data);
}
