import React from 'react';
import {
  get, pick,
} from 'lodash';
import { toJS } from 'mobx';
import { IFieldProcessConfig } from '../base/type';
import { getComponentCodeForLocalCode, getFieldPropsByMode } from '../base/utils';
import { getSearchFieldPropsByFieldType, IAgileBaseSearchFieldInstance, wrapDateToFlatDate } from './search';
import getFieldsInstance, { AgileComponentMap } from '../base';
import SelectProject from '@/components/select/select-project';
import SelectIssueType from '@/components/select/select-issue-type';
import SelectStatus from '@/components/select/select-status';
import SelectUser from '@/components/select/select-user';
import SelectWorkbenchPriority from '@/components/select/select-workbench-priority';

function getFieldConfig({
  field, props,
}: any) {
  const { fieldType, value } = field;
  const defaultValue = toJS(value);
  const { projectId, applyType } = props;
  const code = getComponentCodeForLocalCode(field.code);
  const otherProps = {
    ...getFieldPropsByMode({
      code, outputs: ['config', 'function'], fieldType, props,
    }),
    ...getSearchFieldPropsByFieldType(fieldType, field.id),
  };
  switch (code) {
    case 'status': {
      return {
        code,
        props: {
          ...otherProps,
          defaultSelectedIds: defaultValue,
          isWorkBench: true,
        },
      };
    }
    case 'issueType': {
      return {
        code,
        props: {
          ...otherProps,
          defaultSelectedIds: defaultValue,
          level: 'workbench',
        },
      };
    }
    case 'projectIds': {
      return {
        code: 'project',
        props: {
          ...otherProps,
          defaultSelectedIds: defaultValue,
          level: 'workbench',
        },
      };
    }

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
          selected: defaultValue,
          level: 'workbench',
          request: undefined,
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
  priority: SelectWorkbenchPriority,
  ...pick(AgileComponentMap, ['estimatedStartTime', 'estimatedEndTime', 'actualStartTime', 'actualEndTime']),
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
  return fieldInstance(fieldConfigs, [], []).map((i: any[]) => {
    const element = ['date', 'time', 'datetime'].includes(i[0].fieldType) && i[0].props.flat ? wrapDateToFlatDate(i[0], i[1]) : i[1](i[0]);
    return element;
  }) as React.ReactElement[];
}
export { AgileBaseSearchInstance };
export default getSearchWorkbenchFields;
