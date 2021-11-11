import React from 'react';
import {
  get, merge, omit,
} from 'lodash';
import moment from 'moment';
import getFieldsInstance, { getAgileFields } from '../base';
import { IFieldProcessConfig } from '../base/type';
import { getComponentCodeForLocalCode, getFieldPropsByMode, isCodeInSystemComponents } from '../base/utils';

function getFieldConfig({
  field, props,
}: any) {
  const { fieldType } = field;
  const { projectId, applyType, value } = props;
  const code = getComponentCodeForLocalCode(field.code);
  let newProps: any = {};
  switch (fieldType) {
    case 'input': {
      newProps = { style: { width: 100 } };
      break;
    }
    case 'number': {
      newProps = { style: { width: 100 } };
      break;
    }
    case 'single':
    case 'multiple':
    case 'radio':
    case 'checkbox': {
      newProps = {
        onlyEnabled: false,
        fieldId: field.id,
        // 这里不使用selected 字段内部会处理 value 第二页情况
      };
      break;
    }
  }
  if (isCodeInSystemComponents(code)) {
    return {
      props: getFieldPropsByMode({
        code, outputs: ['config', 'function'], fieldType, props,
      }),
    };
  }
  return {
    props: {
      ...getFieldPropsByMode({
        code, outputs: ['config', 'function'], fieldType, props,
      }),
      ...newProps,
    },
  };
}

const AgileBaseSearchInstance: IAgileBaseSearchFieldInstance = {
  fieldInstance: getAgileFields,
  configInstance: getFieldConfig,
};
const fieldInstanceHelpType = getFieldsInstance<any, any, any>();
export interface IAgileBaseSearchFieldInstance {
  fieldInstance: typeof fieldInstanceHelpType
  configInstance: (field: { field: any, props: any }) => IFieldProcessConfig<any, any> | {}
}
/**
   *  获取搜索的字段
   * @param fields
   * @param fieldCodeProps IFieldConfig<AgileComponentMapProps, CustomComponentMapProps>[]
   * @param instance 获取字段实例
   */
function getSearchOrgFields(fields: any[], fieldCodeProps?: Record<string, any>, instance = AgileBaseSearchInstance) {
  const { fieldInstance, configInstance } = instance;
  const fieldConfigs = fields.map((field) => {
    const codeProps = get(fieldCodeProps, field.code) || {};
    const props = merge({
      key: field.code,
      label: field.name,
      placeholder: field.name,
      flat: true,
    }, codeProps);
    const config = configInstance({ field, props }) as IFieldProcessConfig<any, any>;
    return {
      code: getComponentCodeForLocalCode(config.code ?? field.code),
      fieldType: field.fieldType,
      outputs: ['config', 'function'] as ['config', 'function'],
      props: {
        ...props,
        ...config.props,
      },
    };
  });
  return [<div>组织字段</div>];
  // return fieldInstance(fieldConfigs, [], []).map((i: any[]) => {
  //   if (['date', 'time', 'datetime'].includes(i[0].fieldType)) {
  //     return wrapDateToFlatDate(i[0], i[1]);
  //   }
  //   return i[1](i[0]);
  // }) as React.ReactElement[];
}
export { AgileBaseSearchInstance };
export default getSearchOrgFields;
