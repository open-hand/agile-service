import React from 'react';
import { stores } from '@choerodon/boot';
import { find } from 'lodash';
import { Tooltip } from 'choerodon-ui';
import humanize from './humanizeDuration';

const { AppState } = stores;

export const getIsOrganization = () => AppState.currentMenuType.type === 'organization';
export const getProjectId = () => (AppState.currentMenuType ? AppState.currentMenuType.id : 0);
export const getProjectName = () => (AppState.currentMenuType ? AppState.currentMenuType.name : '');
export const getOrganizationId = () => (AppState.currentMenuType
  ? AppState.currentMenuType.organizationId
  : 0);
export const getApplyType = () => ((AppState.currentMenuType.categories || []).map((c) => c.code).includes('N_PROGRAM') ? 'program' : 'agile');
export const getMenuType = () => (AppState.currentMenuType ? AppState.currentMenuType.type : '');
export const getIsProjectMember = () => {
  if (AppState.userInfo.currentRoleLabels && Array.isArray(AppState.userInfo.currentRoleLabels)) {
    return AppState.userInfo.currentRoleLabels.includes('PROJECT_MEMBER')
      || AppState.userInfo.currentRoleLabels.includes('PROJECT_ADMIN');
  }
  return false;
};

// 选择主题
export function configTheme({
  list = [],
  textField = 'text',
  valueFiled = 'value',
  primary = false,
  renderText,
  parseNumber = false,
} = {}) {
  const renderPlaceHolder = (ommittedValues) => {
    const values = [];
    for (const value of ommittedValues) {
      // eslint-disable-next-line no-restricted-globals
      const target = parseNumber
        ? find(list, { [valueFiled]: isNaN(value) ? value : Number(value) })
        : find(list, { [valueFiled]: value });
      if (target) {
        if (renderText) {
          values.push(renderText(target));
        } else {
          values.push(target[textField]);
        }
      }
    }
    return <Tooltip title={values.join(', ')}>{values.join(', ')}</Tooltip>;
  };
  return {
    className: `SelectTheme ${primary ? 'primary' : ''}`,
    maxTagCount: 0,
    maxTagPlaceholder: renderPlaceHolder,
  };
}

// 获取文件名后缀
export function getFileSuffix(fileName) {
  return fileName.replace(/.+\./, '').toLowerCase();
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
