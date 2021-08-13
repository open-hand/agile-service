import { castArray } from 'lodash';
import type {
  IFieldConfig, IFieldOutput, IComponentFCWithClassObject, ISingleOrArray, IFieldCustomConfig, IFieldSystemConfig,
} from './type';
import { getProcessFieldConfig } from './utils';
import getElement, {
  AgileComponentMapProps, CustomComponentMapProps, AgileComponentMap, CustomFieldMap,
} from './component';
import { IFieldType } from '@/common/types';

interface IFieldsInstanceConfig {
  SystemComponents?: IComponentFCWithClassObject
  CustomComponents?: IComponentFCWithClassObject
}
interface IFieldBase {
  code: string
  fieldType: IFieldType
}

/**
 * 获取字段配置函数实例
 * @param config @default 基础敏捷配置
 *
 */
function getFieldsInstance<T extends IFieldBase = IFieldBase, S extends IComponentFCWithClassObject = AgileComponentMapProps,
  C extends IComponentFCWithClassObject = CustomComponentMapProps>(config: IFieldsInstanceConfig = {}) {
  const { SystemComponents = AgileComponentMap, CustomComponents = CustomFieldMap } = config;
  /**
 * 获取字段配置/元素/自定义配置渲染
 * @param fields
 * @param systemFields
 * @param customFields
 * @returns
 */
  function getFields(
    fields: ISingleOrArray<T> = [], systemFields: ISingleOrArray<IFieldSystemConfig<S>> = [], customFields: ISingleOrArray<IFieldCustomConfig<C>> = [],
  ): Array<Array<IFieldOutput<S | C>>> {
    const fieldsArray = castArray(fields).map((item) => ({
      code: item.code,
      fieldType: item.fieldType,
      outputs: ['element'],
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
          return (newFieldConfig: any) => getElement(newFieldConfig, SystemComponents, CustomComponents);
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
const getAgileFields = getFieldsInstance<IFieldBase & { [x: string]: any }, AgileComponentMapProps, CustomComponentMapProps>();
// function getAgileFields<T extends IFieldBase = IFieldBase>(fields: ISingleOrArray<T> = [], systemFields: ISingleOrArray<IFieldSystemConfig<AgileComponentMapProps>> = [],
//   customFields: ISingleOrArray<IFieldCustomConfig<CustomComponentMapProps>> = []) {
//   return getFields(fields, systemFields, customFields);
// }
// getAgileFields([], [{ code: 'sprint', outputs: ['element'], props: { hasUnassign: true } }]);
export default getFieldsInstance;
export { getAgileFields, AgileComponentMap, CustomFieldMap };
