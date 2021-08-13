import { find } from 'lodash';
import React from 'react';
import {
  TextField, Select, DatePicker, TimePicker, DateTimePicker, NumberField, TextArea, UrlField, DataSet, CheckBox,
} from 'choerodon-ui/pro';

import SelectStatus from '@/routes/StateMachine/components/select-status';
import SelectPI from '@/components/select/select-pi';
import SelectCustomField from '../../select/select-custom-field';
import SelectEnvironment from '../../select/select-environment';
import SelectIssueType from '../../select/select-issue-type';
import SelectMultiServiceTag from '../../select/select-multi-service-tag';
import SelectProgramVersion from '../../select/select-program-version';
import SelectSprint from '../../select/select-sprint';
import SelectSubProject from '../../select/select-sub-project';
import SelectUser from '../../select/select-user';
import type {
  IFieldOutput, IFieldProcessConfig, ICodeDistributeProps, IFieldTypeDistributeProps, IComponentFCWithClassObject, IFieldCustomComponentConfig, IComponentFCWithClass, IComponentFCObject, IFieldSystemComponentConfig, IFieldComponentConfig,
} from './type';
import SelectEpic from '../../select/select-epic';
import SelectLabel from '../../select/select-label';
import SelectPriority from '../../select/select-priority';
import SelectComponent from '../../select/select-component';
import SelectVersion from '../../select/select-version';
import QuickFilterField from './components/quick-filter-field';
import { IFieldType } from '@/common/types';
import { validKeyReturnValue } from '@/common/commonValid';
import SelectSubFeature from '@/components/select/select-sub-feature';

const AgileComponentMap = {
  sprint: SelectSprint,
  status: SelectStatus,
  epic: SelectEpic,
  label: SelectLabel,
  priority: SelectPriority,
  component: SelectComponent,
  version: SelectVersion,
  environment: SelectEnvironment,
  issueType: SelectIssueType,
  feature: SelectSubFeature, // 子项目查询特征
  tag: SelectMultiServiceTag,
  pi: SelectPI,
  programVersion: SelectProgramVersion,
  subProject: SelectSubProject,
  quickFilter: QuickFilterField,
};
export type AgileComponentMapProps = typeof AgileComponentMap

const CustomFieldMap = {
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
export type CustomFCComponentMapProps = Pick<typeof CustomFieldMap, 'radio' | 'checkbox' | 'single' | 'multiMember' | 'multiple' | 'member'>
interface ClassType<T> extends Function {
  new(...args: any[]): T;
}
export interface CustomComponentMapProps extends CustomFCComponentMapProps {
  time: ClassType<TimePicker>
  datetime: ClassType<DateTimePicker>
  date: ClassType<DatePicker>
  number: ClassType<NumberField>
  input: ClassType<TextField>,
  text: ClassType<TextArea<any>>
}
/**
 *  获取默认空元素
 * @param processConfig
 * @returns
 */
function getDefaultEmptyElement() {
  return TextField;
}

function getSystemsElement<T extends IComponentFCObject>(filedProcessConfig: IFieldSystemComponentConfig<T>, components: T) {
  return validKeyReturnValue(filedProcessConfig.code, components) as React.FC;
}
function getCustomElement<T extends IComponentFCWithClassObject>(filedProcessConfig: IFieldCustomComponentConfig<T>, components: T) {
  return validKeyReturnValue(filedProcessConfig.fieldType, components) as IComponentFCWithClass;
}
/**
 * 获取组件元素
 * @param filed
 * @returns
 */
function getElement(filedProcessConfig: IFieldProcessConfig<typeof AgileComponentMap, CustomComponentMapProps>) {
  let element: IComponentFCWithClass<any, any> = getDefaultEmptyElement();
  if (filedProcessConfig.code && Object.keys(AgileComponentMap).includes(filedProcessConfig.code)) {
    element = getSystemsElement(filedProcessConfig as IFieldSystemComponentConfig<typeof AgileComponentMap>, AgileComponentMap);
  } else if (Object.keys(CustomFieldMap).includes(filedProcessConfig.fieldType)) {
    element = getCustomElement(filedProcessConfig as IFieldCustomComponentConfig<typeof CustomFieldMap>, CustomFieldMap);
  }
  console.log('filedProcessConfig', filedProcessConfig);
  return React.createElement(element, { ...filedProcessConfig.props });
}
// getElement({ fieldType: 'member', code: 'label00', props: {} });
// getElement({ fieldType: 'member', code: 'sprint', props: { hasUnassign: true } });

export default getElement;
