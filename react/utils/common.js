import { stores } from '@choerodon/boot';
import humanize from './humanizeDuration';

const { AppState } = stores;

export const getIsOrganization = () => (AppState.currentMenuType ? AppState.currentMenuType.type === 'organization' : undefined);
export const getProjectId = () => (AppState.currentMenuType ? AppState.currentMenuType.id : 0);
export const getProjectName = () => (AppState.currentMenuType ? AppState.currentMenuType.name : '');
export const getOrganizationId = () => (AppState.currentMenuType
  ? AppState.currentMenuType.organizationId
  : 0);
export const getUserId = () => AppState.userInfo?.id || 0;
export const getApplyType = (mixtureCategory = false) => {
  const codes = (AppState.currentMenuType.categories || []).map((c) => c.code);
  let applyType = codes.includes('N_PROGRAM') ? 'program' : 'agile';
  if (mixtureCategory) {
    applyType = codes.includes('N_PROGRAM') && codes.includes('N_AGILE') ? '' : applyType;
  }
  return applyType;
};
export const getMenuType = () => (AppState.currentMenuType ? AppState.currentMenuType.type : '');
export const getIsProjectMember = () => {
  if (AppState.userInfo.currentRoleLabels && Array.isArray(AppState.userInfo.currentRoleLabels)) {
    return AppState.userInfo.currentRoleLabels.includes('PROJECT_MEMBER')
      || AppState.userInfo.currentRoleLabels.includes('PROJECT_ADMIN');
  }
  return false;
};

// 获取文件名后缀
export function getFileSuffix(fileName) {
  return (fileName || '').replace(/.+\./, '').toLowerCase();
}
/**
 * 时间（毫秒）转文字显示
 * @param {*} ms
 */
export function humanizeDuration(ms, config = {}) {
  return humanize(ms, {
    language: 'zh_CN',
    delimiter: '',
    spacer: '',
    largest: 2,
    round: true,
    ...config,
  });
}
