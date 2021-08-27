import React from 'react';
import {
  DataSet,
} from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { DatePickerProps } from 'choerodon-ui/pro/lib/date-picker/DatePicker';
import { IChosenFieldField } from '@/components/chose-field/types';
import getFilterFields from '@/components/field-pro/layouts/filter';
/**
 * @deprecated 后续请使用 `field-pro`中的`getFilterFields`
 * @param field
 * @param otherComponentProps
 * @param param2
 * @returns
 */
export default function renderField<T extends Partial<SelectProps>>(field: IChosenFieldField, otherComponentProps: T | Partial<DatePickerProps> | any,
  { dataSet, useSelectUserForceRefreshHook }: {
    dataSet?: DataSet, useSelectUserForceRefreshHook?: [any, React.Dispatch<React.SetStateAction<any>>]
  }) {
  return getFilterFields([{ field, dataSet, otherComponentProps }])[0];
}
