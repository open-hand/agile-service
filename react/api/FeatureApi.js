import { axios } from '@choerodon/boot';
import IsInProgramStore from '@/stores/common/program/IsInProgramStore';
import { getProjectId } from '@/utils/common';

export function getSubStoryByFeature(issueId) {
  return axios.post(`/agile/v1/projects/${getProjectId()}/issues/list_story_by_feature_id?issueId=${issueId}`);
}
export function getPiNotDone(data) {
  return axios.post(`/agile/v1/projects/${getProjectId()}/project_invoke_program/pi/query_pi_by_status?programId=${IsInProgramStore.artInfo.programId}`, data);
}
