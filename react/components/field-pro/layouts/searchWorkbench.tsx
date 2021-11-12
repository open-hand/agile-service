import React from 'react';
import {
  get, merge,
} from 'lodash';
import { IFieldProcessConfig } from '../base/type';
import { getComponentCodeForLocalCode, getFieldPropsByMode } from '../base/utils';
import { getSearchFieldPropsByFieldType, IAgileBaseSearchFieldInstance } from './search';
import getFieldsInstance from '../base';
import SelectProject from '@/components/select/select-project';
import SelectIssueType from '@/components/select/select-issue-type';
import SelectStatus from '@/components/select/select-status';
import SelectUser from '@/components/select/select-user';

function getFieldConfig({
  field, props,
}: any) {
  const { fieldType } = field;
  const { projectId, applyType, value } = props;
  const code = getComponentCodeForLocalCode(field.code);
  const otherProps = {
    ...getFieldPropsByMode({
      code, outputs: ['config', 'function'], fieldType, props,
    }),
    ...getSearchFieldPropsByFieldType(fieldType, field.id),
  };
  switch (code) {
    case 'status':
      return {
        ...otherProps,
        isWorkBench: true,
      };
    case 'issueType': {
      console.log('issueType.');
      return {
        ...otherProps,
        request: () => new Promise((r) => ([])),
      }; }
    default:
      break;
  }
  switch (fieldType) {
    case 'multiMember':
    case 'member':
      return {
        code,
        props: {
          ...otherProps,
          level: 'workbench',
        },
      };

    default:
      break;
  }
  return { code, props: otherProps };
}
const AgileWorkBenchComponents = {
  project: SelectProject,
  issueType: SelectIssueType,
  status: SelectStatus,
  assignee: SelectUser,
};
const getAgileWorkBenchFields = getFieldsInstance<any, {}>({ SystemComponents: AgileWorkBenchComponents });
const AgileBaseSearchInstance: IAgileBaseSearchFieldInstance = {
  fieldInstance: getAgileWorkBenchFields,
  configInstance: getFieldConfig,
};

/**
   *  获取搜索的字段 (工作台)
   *  目前默认 flat:true
   * @param fields
   * @param fieldCodeProps IFieldConfig<AgileComponentMapProps, CustomComponentMapProps>[]
   * @param instance 获取字段实例
   */
function getSearchWorkbenchFields(fields: any[], fieldCodeProps?: Record<string, any>, instance = AgileBaseSearchInstance) {
  const { fieldInstance, configInstance } = instance;
  const fieldConfigs = fields.map((field) => {
    const codeProps = get(fieldCodeProps, field.code) || {};
    const props = {
      key: field.code,
      label: field.name,
      placeholder: field.name,
      flat: true,
      ...codeProps,
    };
    const config = configInstance({ field, props }) as IFieldProcessConfig<any, any>;
    return {
      code: config.code ?? field.code,
      fieldType: field.fieldType,
      outputs: ['config', 'function'] as ['config', 'function'],
      props: {
        ...props,
        ...config.props,
      },
    };
  });
  return fieldInstance(fieldConfigs, [], []).map((i: any[]) => i[1](i[0])) as React.ReactElement[];
}
export { AgileBaseSearchInstance };
export default getSearchWorkbenchFields;
