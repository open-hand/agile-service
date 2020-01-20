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
};
