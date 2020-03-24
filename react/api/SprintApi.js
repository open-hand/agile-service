import { axios } from '@choerodon/boot';
import { getProjectId, getOrganizationId } from '../common/utils';
/**
 *
 *
 * @param {*} data {*sprintName,startDate,endDate,sprintGoal}
 * @returns
 */
function create(data) {
  return axios.post(`/agile/v1/projects/${getProjectId()}/sprint`, data);
}


/**
 *
 *
 * @param {*} data {*sprintName,startDate,endDate,sprintGoal}
 * @returns
 */
function createOnCurrentPi(data) {
  return axios.post(`/agile/v1/projects/${getProjectId()}/sprint/sub_project`, data);
}
function currentPiStartSprint(data) {
  return axios.post(`/agile/v1/projects/${getProjectId()}/sprint/sub_project/start`, data);
}
function currentPiUpdateSprint(data) {
  return axios.put(`/agile/v1/projects/${getProjectId()}/sprint/sub_project`, data);
}
/**
 *
 *得到PI信息
 * @returns
 */
export function getCurrentPiInfo(programId, artId) {
  return axios.get(`/agile/v1/projects/${getProjectId()}/pi/query_doing_pi?program_id=${programId}&art_id=${artId}`);
}

/**
 *
 *查询当前项目pi下的所有冲刺
 * @returns
 */
export function getCurrentPiAllSprint(piId) {
  return axios.get(`/agile//v1/projects/${getProjectId()}/sprint/sub_project/list?pi_id=${piId}`);
}
/**
 *
 *
 * @param {*} name
 * @returns
 */
function validate(name) {
  return axios.post(`/agile/v1/projects/${getProjectId()}/sprint/check_name`, {
    sprintName: name,
  });
}

export default {
  create,
  validate,
  createOnCurrentPi,
  currentPiStartSprint,
  currentPiUpdateSprint,
};
