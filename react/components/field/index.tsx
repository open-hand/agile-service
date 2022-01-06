import React, { useMemo, useCallback } from 'react';
import type { Moment } from 'moment';
import { FormFieldProps } from 'choerodon-ui/pro/lib/field/FormField';
import { IFieldType } from '@/common/types';
import { IFilterField } from '../filter';
import { getFieldElement, encodeDate, decodeDate } from './utils';

const transformValue = (fieldType: IFieldType, value: string | [] | undefined) => {
  if (fieldType === 'time' || fieldType === 'datetime' || fieldType === 'date') {
    return encodeDate(value as string, fieldType === 'time');
  }
  return value;
};
interface IFieldProps extends Partial<FormFieldProps> {
  mode?: 'edit' | 'create' | 'filter'
  field: IFilterField
  value?: any
  onChange: (value: any) => void
  render?: (field: IFilterField, element: React.ReactNode) => React.ReactNode
  flat?: boolean
}
/**
 * @deprecated 后续将废弃，融合迁移到 `field-pro`
 * @param param0
 * @returns
 */
const Field: React.FC<IFieldProps> = ({
  field, mode, value, onChange, render, flat, ...otherProps
}) => {
  const { fieldType, title } = field;
  const element = render ? render(field, getFieldElement(field, flat)) : getFieldElement(field, flat);
  const shouldMultipleOnFilter = useMemo(() => mode === 'filter'
    && ['member', 'multiMember', 'radio', 'single', 'checkbox', 'multiple'].includes(fieldType),
  [fieldType, mode]);
  const shouldRangeOnFilter = useMemo(() => mode === 'filter'
    && ['date', 'datetime', 'time'].includes(field.fieldType),
  [field.fieldType, mode]);
  const isMultiple = useMemo(() => ['checkbox', 'multiple'].includes(fieldType) || shouldMultipleOnFilter, [fieldType, shouldMultipleOnFilter]);
  const handleChange = useCallback((v: any) => {
    if (fieldType === 'time' || fieldType === 'datetime' || fieldType === 'date') {
      onChange(decodeDate(v as Moment, fieldType === 'time'));
    } else {
      onChange(v);
    }
  }, [fieldType, onChange]);
  if (element && React.isValidElement(element)) {
    return React.cloneElement(element as React.ReactElement, {
      field,
      label: title,
      multiple: isMultiple,
      range: shouldRangeOnFilter,
      style: { width: '100%' },
      value: transformValue(fieldType, value),
      onChange: handleChange,
      ...otherProps,
    });
  }
  return null;
};
export default Field;
