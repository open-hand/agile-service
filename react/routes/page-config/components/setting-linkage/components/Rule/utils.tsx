import SelectUser from '@/components/select/pro/select-user';
import SelectComponent from '@/components/select/select-component';
import SelectCustomField from '@/components/select/select-custom-field';
import SelectEnvironment from '@/components/select/select-environment';
import SelectPriority from '@/components/select/select-priority';
import SelectSubProject from '@/components/select/select-sub-project';
import SelectVersion from '@/components/select/select-version';
import {
  Select, TimePicker, DateTimePicker, DatePicker, NumberField, TextField, TextArea,
} from 'choerodon-ui/pro';
import React from 'react';

const { Option } = Select;

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
  const { fieldCode, system, id } = field;
  if (system) {
    switch (fieldCode) {
      case 'priority':
        return <SelectPriority multiple name={name} />;
      case 'component':
        return <SelectComponent valueField="componentId" multiple name={name} />;
      case 'fixVersion':
        return <SelectVersion valueField="versionId" multiple statusArr={['version_planning']} name={name} />;
      case 'influenceVersion':
        return <SelectVersion valueField="versionId" multiple name={name} />;
      case 'subProject': {
        return <SelectSubProject multiple name={name} />;
      }
      default:
        break;
    }
  }
  if (!system) {
    return <SelectCustomField name={name} fieldId={id} onlyEnabled={false} />;
  }
  return <Select name={name} />;
};

interface DefaultValueProps {
  field: IChosenField
  name?: string
  fieldOptions: {meaning: string, value: string}[]
}
export const renderDefaultValue = ({ field, name = 'defaultValue', fieldOptions = [] }: DefaultValueProps) => {
  const {
    fieldType,
  } = field;
  switch (fieldType) {
    case 'time': {
      return (
        <TimePicker name={name} />
      );
    }
    case 'datetime':
      return (
        <DateTimePicker name={name} />
      );
    case 'date':
      return (
        <DatePicker />
      );
    case 'number':
      return <NumberField name={name} />;
    case 'input':
      return (
        <TextField
          maxLength={100}
          valueChangeAction={'input' as any}
          name={name}
        />
      );
    case 'text':
      return (
        <TextArea
          rows={3}
          maxLength={255}
          valueChangeAction={'input' as any}
          name={name}
        />
      );
    case 'radio': case 'checkbox': case 'single': case 'multiple': {
      return (
        <Select name={name} multiple={fieldType === 'checkbox' || fieldType === 'multiple'}>
          {fieldOptions.length > 0
            && fieldOptions.map((item) => (
              <Option
                value={item.value}
                key={item.value}
              >
                {
                item.meaning
                }
              </Option>
            ))}
        </Select>
      );
    }
    case 'multiMember':
    case 'member':
    {
      return (
        <SelectUser
          multiple={fieldType === 'multiMember'}
          clearButton
          name={name}
        />
      );
    }
    default: return null;
  }
};
