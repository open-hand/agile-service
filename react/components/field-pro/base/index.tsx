import { castArray } from 'lodash';
import type {
  IFieldConfig, IFieldOutput, IComponentFCWithClassObject, ISingleOrArray, IFieldCustomConfig, IFieldSystemConfig, IFieldConfigOutputType,
} from './type';
import { getProcessFieldConfig } from './utils';
import getElement, {
  AgileComponentMap, CustomFieldMap,
} from './component';
import type { AgileComponentMapProps, CustomComponentMapProps } from './component';
import { IFieldType } from '@/common/types';

interface IFieldsInstanceConfig {
  SystemComponents?: IComponentFCWithClassObject
  CustomComponents?: IComponentFCWithClassObject
}
export interface IFieldBaseConfig {
  code: string
  fieldType: IFieldType
  display?: boolean
  outputs?: Array<IFieldConfigOutputType>
  props?: any
}

/**
 * 获取字段配置函数实例
 * @param config @default 基础敏捷配置
 *
 */
function getFieldsInstance<T extends IFieldBaseConfig = IFieldBaseConfig, S extends IComponentFCWithClassObject = AgileComponentMapProps,
  C extends IComponentFCWithClassObject = CustomComponentMapProps>(config: IFieldsInstanceConfig = {}) {
  const { SystemComponents = AgileComponentMap, CustomComponents = CustomFieldMap } = config;
  /**
 * 获取字段配置/元素/自定义配置渲染
 * @param fields 系统/自定义字段
 * @param  {Array<IFieldSystemConfig>|IFieldSystemConfig} systemFields 系统字段 有TS提示
 * @param {Array<IFieldCustomConfig>|IFieldCustomConfig} customFields  自定义字段 有TS提示
 * @returns {Array<Array<IFieldOutput<自定义|系统字段>>>}
 */
  function getFields(
    fields: ISingleOrArray<T> = [], systemFields: ISingleOrArray<IFieldSystemConfig<S>> = [], customFields: ISingleOrArray<IFieldCustomConfig<C>> = [],
  ): Array<Array<IFieldOutput<S | C>>> {
    const fieldsArray = castArray(fields).map((item) => ({
      props: item.props ?? {},
      code: item.code,
      fieldType: item.fieldType,
      display: item.display,
      outputs: item.outputs ?? ['element'],
    }) as IFieldConfig<S, C>);
    const systemFieldsArray = castArray(systemFields);
    const customFieldsArray = castArray(customFields);
    const fieldConfigs = [...fieldsArray, ...systemFieldsArray, ...customFieldsArray];
    const outputs = fieldConfigs.map((filedConfig) => {
      const fieldConfig = getProcessFieldConfig<S, C>(filedConfig);
      const filedOutput = filedConfig.outputs.map((item) => {
        if (item === 'config') {
          return fieldConfig;
        }
        if (item === 'element') {
          return getElement(fieldConfig as any, SystemComponents, CustomComponents);
        }
        if (item === 'function') {
          return (newFieldConfig: any, element?: any) => element || getElement(newFieldConfig, SystemComponents, CustomComponents);
        }
        return undefined;
      });
      return filedOutput.filter(Boolean) as Array<IFieldOutput<S | C>>;
    });
    return outputs;
  }
  return getFields;
}

/**
 * 获取基础敏捷字段配置/元素/自定义配置渲染
 * @param fields
 * @param systemFields
 * @param customFields
 * @returns
 */
const getAgileFields = getFieldsInstance<IFieldBaseConfig & { [x: string]: any }, AgileComponentMapProps, CustomComponentMapProps>();
// function getAgileFields<T extends IFieldBase = IFieldBase>(fields: ISingleOrArray<T> = [], systemFields: ISingleOrArray<IFieldSystemConfig<AgileComponentMapProps>> = [],
//   customFields: ISingleOrArray<IFieldCustomConfig<CustomComponentMapProps>> = []) {
//   return getFields(fields, systemFields, customFields);
// }
// getAgileFields([], [{ code: 'sprint', outputs: ['element'], props: { hasUnassign: true } }]);
export default getFieldsInstance;
export { getAgileFields, AgileComponentMap, CustomFieldMap };
