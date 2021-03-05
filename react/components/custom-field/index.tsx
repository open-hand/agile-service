import React, { forwardRef } from 'react';
import moment from 'moment';
import { omit } from 'lodash';
import SelectUser from '@/components/select/select-user';
import {
  TextField, TextArea, Select, NumberField, DatePicker, DateTimePicker, TimePicker, SelectBox,
} from 'choerodon-ui/pro';
import { IFieldType } from '@/common/types';
import { FormFieldProps } from 'choerodon-ui/pro/lib/field/FormField';
import TextEditToggle from '@/components/TextEditTogglePro';
// import SelectUserOutSide from './select-user-outside';
import { Action } from '../TextEditTogglePro/TextEditToggle';
import UserTag from '../tag/user-tag';

export { IFieldType };
const getEditorByFieldType = (fieldType: IFieldType, outside: boolean) => {
  switch (fieldType) {
    case 'text':
      return <TextArea />;
    case 'input':
      return <TextField />;
    case 'member':
    case 'multiMember':
      // return outside ? <SelectUserOutSide /> : <SelectUser />;
      return <SelectUser multiple={fieldType === 'multiMember'} />;
    case 'single':
      return <Select />;
    case 'multiple':
      return <Select />;
    case 'radio':
      return <SelectBox />;
    case 'checkbox':
      return <SelectBox />;
    case 'number':
      return <NumberField />;
    case 'time':
      return <TimePicker />;
    case 'date':
      return <DatePicker />;
    case 'datetime':
      return <DateTimePicker />;
    default:
      return null;
  }
};
interface IFieldOption {
  id: string
  value: string
  enabled: boolean
}
export interface IField {
  code: string
  fieldName: string
  fieldId: string
  value: string
  fieldType: IFieldType
  fieldCode: string
  valueStr: string
  required: boolean
  fieldOptions?: IFieldOption[]
  system: boolean
  defaultValue: string | null
  defaultValueObj?: Object
}
interface Props extends FormFieldProps {
  field: IField
  outside?: boolean
  mode?: 'edit' | 'create'
  onSubmit?: (value: any, field: IField) => void
}

const transformValue = (fieldType: IFieldType, value: string | []) => {
  if (fieldType === 'time' || fieldType === 'datetime' || fieldType === 'date') {
    return value ? moment(value) : undefined;
  } if (value instanceof Array) {
    return value.slice();
  }
  return value;
};
const CustomField: React.FC<Props> = forwardRef(({
  field, mode = 'create', disabled, onSubmit, outside = false, ...otherProps
}, ref) => {
  const {
    value, fieldType, valueStr, required,
  } = field;
  const submitTrigger: Action[] = ['blur'] as Action[];
  const submitOnChange = ['member', 'single', 'radio'].includes(fieldType);
  if (submitOnChange) {
    submitTrigger.push('change' as Action);
  }
  const submitOnOut = ['radio', 'checkbox'].includes(fieldType);
  if (submitOnOut) {
    submitTrigger.push('click' as Action);
  }
  const handleSubmit = (newValue: any) => {
    let submitValue = newValue;
    if (fieldType === 'time' || fieldType === 'datetime' || fieldType === 'date') {
      submitValue = newValue && newValue.format('YYYY-MM-DD HH:mm:ss');
    }
    if (onSubmit) {
      onSubmit(submitValue, field);
    }
  };
  const editor = getEditorByFieldType(fieldType, outside);

  if (editor) {
    let props = {};
    switch (fieldType) {
      case 'single':
      case 'multiple':
      case 'radio':
      case 'checkbox':
      {
        const options = field.fieldOptions && field.fieldOptions.length > 0
            && field.fieldOptions.filter((option) => option.enabled
              || (value && value.indexOf(option.id) !== -1)).map((item) => (
                <Select.Option
                  value={item.id}
                  key={item.id}
                >
                  {item.value}
                </Select.Option>
            ));
        props = {
          children: options,
          required,
          multiple: fieldType === 'multiple' || fieldType === 'checkbox',
        };
        break;
      }
      case 'text': {
        props = {
          required,
          autoSize: true,
        };
        break;
      }
      default: {
        props = {
          required,
        };
        break;
      }
    }
    if (mode === 'create') {
      return React.cloneElement(editor, { ...props, ...otherProps, ref });
    }
    return (
      <TextEditToggle
        className={otherProps.className}
        disabled={disabled}
        alwaysRender={!['time', 'date', 'datetime'].includes(fieldType)}
        onSubmit={handleSubmit}
        initValue={transformValue(fieldType, value)}
        editor={() => React.cloneElement(editor, { ...props, ...omit(otherProps, 'className'), ref })}
        submitTrigger={submitTrigger}
      >
        <div style={{ maxWidth: 200, wordBreak: 'break-all', whiteSpace: 'pre-line' }}>
          {['member', 'multiMember'].includes(fieldType) && valueStr
            ? (
              <UserTag
                // @ts-ignore
                user={valueStr}
              />
            ) : (valueStr || '无')}
        </div>
      </TextEditToggle>
    );
  }
  return null;
});

export default CustomField;
