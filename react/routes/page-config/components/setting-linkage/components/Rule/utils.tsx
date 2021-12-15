import {
  Select,
} from 'choerodon-ui/pro';
import React from 'react';
import {
  set, assign, includes, unset,
} from 'lodash';
import moment from 'moment';
import { FORMAT_FIELDS } from '@/constants/DATE_FORMAT';
import { IAgileBaseSearchFieldInstance } from '@/components/field-pro/layouts/search';

export interface IChosenField {
  name: string,
  id: string,
  code: string,
  system: boolean,
  fieldType: string,
  fieldCode: string,
}
interface Props {
  field: IChosenField
  name?: string
  getFieldInstance: IAgileBaseSearchFieldInstance['fieldInstance']
}
function isSelect(fieldType: string) {
  return includes(['radio', 'multiple', 'checkbox', 'single'], fieldType);
}
export const renderFieldRelSelect = ({ field, name = 'fieldRelOptionList', getFieldInstance }: Props) => {
  const { system, id } = field;
  const fieldCode = field.fieldCode as 'priority' | 'component' | 'fixVersion' | 'influenceVersion' | 'subProject';
  if (system) {
    const props = { name, multiple: true };
    switch (fieldCode) {
      case 'fixVersion': {
        set(props, 'valueField', 'versionId');
        set(props, 'statusArr', ['version_planning']);
        break;
      }
      case 'influenceVersion': {
        set(props, 'valueField', 'versionId');
        break;
      }
      default:
        break;
    }
    return getFieldInstance([], { code: fieldCode, outputs: ['element'], props })[0][0];
  }
  if (!system) {
    return getFieldInstance([], [], {
      fieldType: 'single', outputs: ['element'], props: { key: id, name, fieldId: id },
    })[0][0];
  }
  return <Select name={name} />;
};

interface DefaultValueProps {
  field: IChosenField
  name?: string
  fieldOptions: { meaning: string, value: string }[]
  getFieldInstance: IAgileBaseSearchFieldInstance['fieldInstance']

}
export const renderDefaultValue = ({
  field, name = 'defaultValue', fieldOptions = [], getFieldInstance,
}: DefaultValueProps) => {
  const {
    fieldType,
  } = field;

  // 预计开始/结束时间、实际开始/结束时间精确到分
  const code = field.fieldCode && FORMAT_FIELDS.includes(field.fieldCode) ? field.fieldCode : field.code ?? field.fieldCode as string;
  const fieldProps = {
    name,
    fieldOptions: fieldOptions.map((i) => ({ id: i.value, value: i.meaning, enabled: true })),
  };
  if (field.fieldCode && ['estimatedEndTime', 'actualEndTime'].includes(field.fieldCode)) {
    assign(fieldProps, {
      defaultPickerValue: moment().endOf('d'),
    });
  }
  if (fieldOptions.length === 0 && isSelect(fieldType)) {
    unset(fieldProps, 'fieldOptions');
    set(fieldProps, 'fieldId', field.id);
  }
  return getFieldInstance([], [], {
    code,
    fieldType: fieldType as any,
    // 初次进来没有选项显示值时不显示
    // display: !!fieldOptions.filter((i) => i.meaning).length,
    outputs: ['element'],
    props: fieldProps,
  })[0][0];
};
