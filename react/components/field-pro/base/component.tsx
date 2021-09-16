import { find } from 'lodash';
import React from 'react';
import {
  TextField, Select, DatePicker, TimePicker, DateTimePicker, NumberField, TextArea, UrlField, DataSet, CheckBox,
} from 'choerodon-ui/pro';
import { TextAreaProps } from 'choerodon-ui/pro/lib/text-area/TextArea';
import { FormFieldProps } from 'choerodon-ui/pro/lib/field/FormField';
import SelectStatus from '@/components/select/select-status';
import SelectPI from '@/components/select/select-pi';
import DateTimePickerWithDefault from '@/components/date-time-picker';
import SelectCustomField from '../../select/select-custom-field';
import SelectEnvironment from '../../select/select-environment';
import SelectIssueType from '../../select/select-issue-type';
import SelectMultiServiceTag from '../../select/select-multi-service-tag';
import SelectProgramVersion from '../../select/select-program-version';
import SelectSprint from '../../select/select-sprint';
import SelectSubProject from '../../select/select-sub-project';
import SelectUser from '../../select/select-user';
import type {
  IFieldOutput, IFieldProcessConfig, IClassComponentType, ICodeDistributeProps, IFieldTypeDistributeProps,
  IComponentFCWithClassObjectProps,
  IComponentFCWithClassObject, IFieldCustomComponentConfig, IComponentFCWithClass, IComponentFCObject, IFieldSystemComponentConfig, IFieldComponentConfig, IFieldSystemConfig,
} from './type';
import SelectEpic from '../../select/select-epic';
import SelectLabel from '../../select/select-label';
import SelectPriority from '../../select/select-priority';
import SelectComponent from '../../select/select-component';
import SelectVersion from '../../select/select-version';
import SelectQuickFilterField from '@/components/select/select-quick-filter';
import { IFieldType } from '@/common/types';
import { validKeyReturnValue } from '@/common/commonValid';
import SelectSubFeature from '@/components/select/select-sub-feature';
import Editor from '@/components/Editor';

export const AgileComponentMap = {
  sprint: SelectSprint,
  status: SelectStatus,
  epic: SelectEpic,
  label: SelectLabel,
  priority: SelectPriority,
  component: SelectComponent,
  version: SelectVersion,
  fixVersion: SelectVersion,
  influenceVersion: SelectVersion,
  environment: SelectEnvironment,
  issueType: SelectIssueType,
  feature: SelectSubFeature, // 子项目查询特征
  tag: SelectMultiServiceTag,
  pi: SelectPI,
  programVersion: SelectProgramVersion,
  subProject: SelectSubProject,
  quickFilter: SelectQuickFilterField,
  description: Editor as IClassComponentType<Editor<FormFieldProps>>,
};
const A = { pi: SelectPI };

export type AgileComponentMapProps = typeof AgileComponentMap

export type IAgileBaseComponentPartialProps = Partial<IComponentFCWithClassObjectProps<AgileComponentMapProps>>

export const CustomFieldMap = {
  time: TimePicker,
  datetime: DateTimePicker,
  date: DatePicker,
  number: NumberField,
  input: TextField,
  text: TextArea,
  radio: SelectCustomField,
  checkbox: SelectCustomField,
  single: SelectCustomField,
  multiple: SelectCustomField,
  multiMember: SelectUser,
  member: SelectUser,
};
export interface CustomClassComponentMapProps {
  time: IClassComponentType<TimePicker>
  datetime: IClassComponentType<DateTimePicker>
  date: IClassComponentType<DatePicker>
  number: IClassComponentType<NumberField>
  input: IClassComponentType<TextField>,
  text: IClassComponentType<TextArea<any>>
}
export type CustomComponentMapProps = typeof CustomFieldMap & CustomClassComponentMapProps

export type IAgileBaseFieldTypeComponentProps = Partial<IComponentFCWithClassObjectProps<CustomComponentMapProps>>

/**
 *  获取默认空元素
 * @param processConfig
 * @returns
 */
function getDefaultEmptyElement() {
  return TextField;
}

function getSystemsElement<T extends IComponentFCWithClassObject>(filedProcessConfig: IFieldSystemComponentConfig<T>, components: T) {
  return validKeyReturnValue(filedProcessConfig.code, components) as IComponentFCWithClass;
}
function getCustomElement<T extends IComponentFCWithClassObject>(filedProcessConfig: IFieldCustomComponentConfig<T>, components: T) {
  return validKeyReturnValue(filedProcessConfig.fieldType, components) as IComponentFCWithClass;
}
/**
 * 获取组件元素
 * @param filed
 * @returns
 */
function getElement<T extends IComponentFCWithClassObject,
  C extends IComponentFCWithClassObject = IComponentFCWithClassObject>(filedProcessConfig: IFieldProcessConfig<T, C>,
  systemComponents: T, customComponents: C) {
  let element: IComponentFCWithClass<any, any> = getDefaultEmptyElement();
  if (!filedProcessConfig.display) {
    return <></>;
  }
  if (filedProcessConfig.code && Object.keys(systemComponents).includes(filedProcessConfig.code)) {
    element = getSystemsElement(filedProcessConfig as IFieldSystemComponentConfig<T>, systemComponents);
  } else if (Object.keys(customComponents).includes(filedProcessConfig.fieldType!)) {
    element = getCustomElement<C>(filedProcessConfig as IFieldCustomComponentConfig<C>, customComponents);
  }
  return React.createElement(element, { ...filedProcessConfig.props });
}

export default getElement;
