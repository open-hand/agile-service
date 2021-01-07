import { Button } from 'choerodon-ui';
import {
  Select as SelectPro, Radio, DatePicker, CheckBox, TextField, TextArea, NumberField,
} from 'choerodon-ui/pro/lib';

import React, {
  forwardRef, useCallback, useMemo, useState,
} from 'react';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import SelectUser from '@/components/select/select-user';
import { getMenuType } from '@/utils/common';
import { userApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import SelectPickDate from './select-date-pick';

interface IBaseComponentProps {
  onChange?: (value: any) => void
  onBlur?: () => void
  value?: any
  style?: React.CSSProperties
}

const Select = forwardRef<any, IBaseComponentProps & Partial<SelectProps & { children: any }>>(({
  children, onBlur, multiple, ...otherProps
}, ref) => (
  <SelectPro
    ref={ref}
    {...otherProps}
    trigger={['click'] as any}
    // trigger={multiple ? ['click'] as any : undefined}
    multiple={multiple}
    onChange={(newValue) => {
      console.log('newValue...', newValue, !!(!multiple && otherProps.onChange));
      otherProps.onChange && otherProps.onChange(newValue);
    }}
    onPopupHiddenChange={(hidden) => {
      console.log('blur', hidden, onBlur);
      hidden && onBlur && onBlur();
    }}
  >
    {children}
  </SelectPro>
));

function renderEditor({ record, defaultValue, dataRef }: { record: Record, defaultValue: any, dataRef?: { current: any } }) {
  const fieldType = record.get('fieldType');
  if (['date', 'time', 'datetime'].includes(fieldType)) {
    return <SelectPickDate dateType={fieldType} />;
  }

  if (['checkbox', 'multiple', 'radio', 'single'].includes(fieldType)) {
    const fieldOptions = record.get('fieldOptions') || [];
    return (
      <Select
        multiple={['checkbox', 'multiple'].includes(fieldType)}
      >
        {fieldOptions.map((item: any) => <SelectPro.Option value={item.id || item.tempKey}>{item.value}</SelectPro.Option>)}
      </Select>
    );
  }
  switch (fieldType) {
    case 'input':
      return <TextField maxLength={100} />;
    case 'text':
      return <TextArea rows={3} maxLength={255} />;
    case 'member':
    {
      const type = getMenuType();
      return (
        <SelectUser
          extraOptions={defaultValue ? [defaultValue] : undefined}
          trigger={['click'] as any}
            // autoQueryConfig={{
            //   selectedUserIds: defaultValue || [],
            //   // @ts-ignore
            //   queryUserRequest: async (userId: number) => (type === 'project' ? userApi.getAllInProject('', undefined, userId) : userApi.getAllInOrg('', undefined, userId)),
            // }}
          dataRef={dataRef}
          request={({ filter, page }) => (type === 'project' ? userApi.getAllInProject(filter, page) : userApi.getAllInOrg(filter, page))}
        />
      );
    }
    case 'number': {
      const extraConfig = record.get('extraConfig');
      return <NumberField step={extraConfig ? 0.1 : 1} />;
    }
    default:
      break;
  }
  return <TextField />;
}

export default renderEditor;
