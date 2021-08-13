import React from 'react';
import type {
  IFieldConfig, IFieldOutput, IFieldProcessConfig, IComponentFCWithClassObject, IFieldRequiredConfig, IFieldComponentConfig, IComponentFCObject,
  IFieldSystemComponentConfig, IFieldCustomComponentConfig,
} from './type';
import { getProcessFieldConfig } from './utils';
import getElement, { AgileComponentMapProps, CustomComponentMapProps } from './component';
import { IFieldType } from '@/common/types';

/**
 * 获取字段配置/元素/自定义配置渲染
 * @param fields
 * @returns
 */
function getFields<T extends IComponentFCWithClassObject, C extends IComponentFCWithClassObject = CustomComponentMapProps>(fields: IFieldConfig<T, C>[] | IFieldConfig<T, C>): Array<IFieldOutput<T>>[] {
  const fieldConfigs = Array.isArray(fields) ? fields : [fields];
  const outputs = fieldConfigs.map((filedConfig) => {
    const config = getProcessFieldConfig(filedConfig);
    const filedOutput = filedConfig.outputs.map((item) => {
      if (item === 'config') {
        return config;
      }
      if (item === 'element') {
        return getElement(config as any);
      }
      if (item === 'function') {
        return (newConfig: any) => getElement(newConfig);
      }
      return undefined;
    });
    return filedOutput.filter(Boolean) as Array<IFieldOutput<T>>;
  });
  return outputs;
}
// getFields({
//   code: 'mmmm', fieldType: 'time', props: {}, outputs: ['element'],
// });

function getDemoFields(fields: IFieldConfig<AgileComponentMapProps, CustomComponentMapProps>[] | IFieldConfig<AgileComponentMapProps, CustomComponentMapProps>) {
  return getFields(fields);
}
type UnionCode<T extends string, K extends string> = T & K

type IFieldCodeUnionFieldTypeComponentConfig<T extends IComponentFCWithClassObject,
  F extends IComponentFCWithClassObject = IComponentFCWithClassObject, SC = keyof T> = IFieldSystemComponentConfig<T> | IFieldCustomComponentConfig<F> // { code: string, fieldType: 'member', props?: React.ComponentProps<CustomComponentMapProps['member']> }
function getB(field: IFieldCodeUnionFieldTypeComponentConfig<AgileComponentMapProps, CustomComponentMapProps>) {

}
// getB({
//   // code: 'mmm', fieldType: 'date', props: { mode: '' as any }, isCustom: true,
// });
// getB({
//   code: 'mmm', fieldType: 'multiMember', props: { selected: ['999'], selectedUser: {} as any }, isCustom: true,
// });
// getB({
//   code: 'mmm', fieldType: 'member', props: { selected: ['999'], selectedUser: {} as any }, isCustom: true,
// });
// getB({ code: 'sprint', fieldType: 'multiple', props: { hasUnassign: true } });
// getDemoFields({ fieldType: 'member', props: { selected: [] as string[] }, });
export default getDemoFields;
