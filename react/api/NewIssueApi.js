import { stores, axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

const { AppState } = stores;


export function loadChartData(id, type) {
  const projectId = AppState.currentMenuType.id;
  return axios.get(`/agile/v1/projects/${projectId}/reports/${id}/burn_down_report?type=${type}&ordinalType=asc`);
}

export function updateIssueWSJFDTO(data, projectId = AppState.currentMenuType.id) {
  return axios.post(`/agile/v1/projects/${projectId}/wsjf`, data);
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


export function createFeatureLink(data) {
  const projectId = AppState.currentMenuType.id;
  return axios.post(`/agile/v1/projects/${projectId}/board_depend/batch_create_depend`, data);
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
