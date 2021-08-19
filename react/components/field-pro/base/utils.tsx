import { merge } from 'lodash';
import {
  IComponentFCWithClassObject, IFieldConfig, IFieldProcessConfig,
} from './type';
import type { CustomComponentMapProps } from './component';
/**
 * 获取处理后的配置
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
// export type IFieldProcessConfigFn=ReturnType<(<T extends IFiledMapProps, K extends keyof T>(fieldConfig: IFieldConfig<T, K>) =>typeof getProcessFieldConfig)>
export { getProcessFieldConfig };
