import {
  Select,
} from 'choerodon-ui/pro';
import React from 'react';
import { set } from 'lodash';
import { getAgileFields } from '@/components/field-pro';

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
}

export const renderFieldRelSelect = ({ field, name = 'fieldRelOptionList' }: Props) => {
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
    return getAgileFields([], { code: fieldCode, outputs: ['element'], props })[0][0];
  }
  if (!system) {
    return getAgileFields([], [], {
      fieldType: 'single', outputs: ['element'], props: { key: id, name, fieldId: id },
    })[0][0];
  }
  return <Select name={name} />;
};

interface DefaultValueProps {
  field: IChosenField
  name?: string
  fieldOptions: { meaning: string, value: string }[]
}
export const renderDefaultValue = ({ field, name = 'defaultValue', fieldOptions = [] }: DefaultValueProps) => {
  const {
    fieldType,
  } = field;

  return getAgileFields([], [], {
    code: field.code ?? field.fieldCode as string,
    fieldType: fieldType as any,
    // 初次进来没有选项显示值时不显示
    // display: !!fieldOptions.filter((i) => i.meaning).length,
    outputs: ['element'],
    props: {
      name,
      fieldOptions: fieldOptions.map((i) => ({ id: i.value, value: i.meaning, enabled: true })),
    },
  })[0][0];
};
