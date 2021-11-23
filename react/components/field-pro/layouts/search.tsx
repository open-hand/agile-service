import React from 'react';
import {
  get, merge, omit,
} from 'lodash';
import moment from 'moment';
import getFieldsInstance, { getAgileFields } from '../base';
import { IFieldProcessConfig } from '../base/type';
import { getComponentCodeForLocalCode, getFieldPropsByMode, isCodeInSystemComponents } from '../base/utils';
import { IFieldType } from '@/common/types';
import { IAgileBaseFieldTypeComponentProps } from '../base/component';

type getSearchFieldPropsByFieldTypeType<T extends IAgileBaseFieldTypeComponentProps = IAgileBaseFieldTypeComponentProps, P extends keyof T = keyof T> = (fieldType: P, fieldId: string) => T[P]
/**
 * 根据类型获取高级筛选类组件Props
 */
const getSearchFieldPropsByFieldType: getSearchFieldPropsByFieldTypeType = (fieldType: IFieldType, fieldId: string) => {
  switch (fieldType) {
    case 'input': {
      return {
        style: { width: 100 },
      };
    }
    case 'number': {
      return {
        style: { width: 100 },
      };
    }
    case 'single':
    case 'multiple':
    case 'radio':
    case 'checkbox': {
      return {
        onlyEnabled: false,
        fieldId,
      };
    }
  }
  return {};
};
function getFieldConfig({
  field, props,
}: any) {
  const { fieldType } = field;
  const code = getComponentCodeForLocalCode(field.code);
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
      ...getSearchFieldPropsByFieldType(fieldType, field.id),
    },
  };
}
export function wrapDateToFlatDate(fieldConfig: any, wrapElementFn: (config: any) => JSX.Element): JSX.Element {
  const { fieldType } = fieldConfig;
  class FlatDateRangePicker extends React.PureComponent<any, any> {
    static getSelectedDate = (value: any[]) => {
      if (!value || value.length === 0 || (!value[0] && !value[1])) {
        return { start: undefined, end: undefined };
      }
      if (fieldType === 'time') {
        return { start: moment(`2000-01-01 ${value[0]}`), end: moment(`2000-01-01 ${value[1]}`) };
      }
      return { start: moment(value[0]), end: moment(value[1]) };
    };

    handleChange = (range: any) => {
      const { onChange } = this.props;
      const { start, end } = range || {};
      if (start && end) {
        if (fieldType === 'time') {
          onChange([
            start.format('HH:mm:ss'),
            end.format('HH:mm:ss'),
          ]);
        } else if (fieldType === 'date') {
          onChange([
            start.startOf('day').format('YYYY-MM-DD HH:mm:ss'),
            end.endOf('day').format('YYYY-MM-DD HH:mm:ss'),
          ]);
        } else {
          onChange([
            start.format('YYYY-MM-DD HH:mm:ss'),
            end.format('YYYY-MM-DD HH:mm:ss'),
          ]);
        }
      } else {
        onChange([]);
      }
    }

    static getDateProps() {
      let width: number = 0;
      if (fieldType === 'datetime') {
        width = 380;
      } else if (fieldType === 'date') {
        width = 265;
      } else if (fieldType === 'time') {
        width = 230;
      }

      return {
        labelLayout: 'float',
        style: { width, margin: '6px 0' },
        placeholder: ['开始时间', '结束时间'],
        range: ['start', 'end'],
      };
    }

    render() {
      return (
        <div className="c7n-pro-form-float">
          {wrapElementFn(merge(omit(fieldConfig, 'props'), {
            props: {
              ...this.props,
              ...FlatDateRangePicker.getDateProps(),
              value: FlatDateRangePicker.getSelectedDate(this.props.value),
              onChange: this.handleChange,
            },
          }))}
        </div>
      );
    }
  }
  return React.createElement(FlatDateRangePicker, { ...fieldConfig.props });
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
function getSearchFields(fields: any[], fieldCodeProps?: Record<string, any>, instance = AgileBaseSearchInstance) {
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
  return fieldInstance(fieldConfigs, [], []).map((i: any[]) => {
    if (['date', 'time', 'datetime'].includes(i[0].fieldType)) {
      return wrapDateToFlatDate(i[0], i[1]);
    }
    return i[1](i[0]);
  }) as React.ReactElement[];
}
export { AgileBaseSearchInstance, getSearchFieldPropsByFieldType };
export default getSearchFields;
