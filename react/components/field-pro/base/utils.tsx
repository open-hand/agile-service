import { merge } from 'lodash';
import {
  IComponentFCWithClassObject, IFieldConfig, IFieldProcessConfig,
} from './type';
import type { CustomComponentMapProps } from './component';
import { AgileComponentMap } from './component';

const filterModeSelectCommonProps = {
  multiple: true,
  maxTagCount: 3,
  maxTagTextLength: 10,
  dropdownMatchSelectWidth: false,
  clearButton: true,
};
const filterModeProps = {
  input: {},
  time: {},
  datetime: {},
  date: {},
  number: {},
  text: {
    props: {
      autoSize: true, rows: 3, maxLength: 255, valueChangeAction: 'input',
    },
  },
  url: {},
  radio: filterModeSelectCommonProps,
  single: filterModeSelectCommonProps,
  checkbox: filterModeSelectCommonProps,
  multiple: filterModeSelectCommonProps,
  multiMember: filterModeSelectCommonProps,
  member: filterModeSelectCommonProps,
};
/**
 * 获取处理后的配置
 * 最终确定字段配置
 */
function getProcessFieldConfig<T extends IComponentFCWithClassObject, C extends IComponentFCWithClassObject = CustomComponentMapProps>(fieldConfig: IFieldConfig<T, C>) {
  const config = {
    code: fieldConfig.code,
    fieldType: fieldConfig.fieldType,
    outputs: fieldConfig.outputs,
    display: fieldConfig.display === undefined || !!fieldConfig.display,
    props: { clearButton: true, name: fieldConfig.code, ...fieldConfig.props },
  };
  const filedTypeConfigObj = {
    time: {},
    datetime: {},
    date: {},
    number: {},
    input: { props: { maxLength: 100, valueChangeAction: 'input' } },
    text: {
      props: {
        autoSize: true, rows: 3, maxLength: 255, valueChangeAction: 'input',
      },
    },
    url: {},
    radio: {},
    single: {},
    checkbox: { props: { multiple: true } },
    multiple: { props: { multiple: true } },
    multiMember: { props: { multiple: true } },
    member: {},
  };
  return merge(config.fieldType ? filedTypeConfigObj[config.fieldType] : {}, config) as unknown as IFieldProcessConfig<T, CustomComponentMapProps>;
}
/**
 * code 是否在系统组件内
 * @param code
 * @param components
 * @returns
 */
function isCodeInSystemComponents(code: string, components: { [x: string]: any } = AgileComponentMap) {
  return Object.keys(components).includes(code);
}
/**
 * 通过模式获取字段的 `props` 配置
 * @param fieldConfig
 * @param mode `filter` 待添加`flatFilter` `create` `edit`
 * @returns
 */
function getFieldPropsByMode(fieldConfig: IFieldConfig<any, any>, mode: 'filter' = 'filter') {
  switch (mode) {
    case 'filter':
      if (fieldConfig.fieldType) {
        return filterModeProps[fieldConfig.fieldType];
      }
  }

  return {};
}
// export type IFieldProcessConfigFn=ReturnType<(<T extends IFiledMapProps, K extends keyof T>(fieldConfig: IFieldConfig<T, K>) =>typeof getProcessFieldConfig)>
export { getProcessFieldConfig, isCodeInSystemComponents, getFieldPropsByMode };
