import React from 'react';
import { stores } from '@choerodon/boot';
import { find } from 'lodash';
import { Tooltip } from 'choerodon-ui';

const { AppState } = stores;

export const getProjectId = () => Number(AppState.currentMenuType ? AppState.currentMenuType.id : 0);
export const getProjectName = () => (AppState.currentMenuType ? AppState.currentMenuType.name : '');
export const getOrganizationId = () => (AppState.currentMenuType ? AppState.currentMenuType.organizationId : 0);

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
      const target = parseNumber ? find(list, { [valueFiled]: isNaN(value) ? value : Number(value) })
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
// 捕获failed异常错误code
export function catchFailed(res) {
  const { failed } = res;
  if (failed) {
    throw res.message;
  } else {
    return res;
  }
}

// 获取文件名后缀
export function getFileSuffix(fileName) {
  return fileName.replace(/.+\./, '').toLowerCase();
}
