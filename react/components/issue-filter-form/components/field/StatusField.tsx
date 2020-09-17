import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import {
  issueTypeApi, IStatusCirculation, statusApi, statusTransformApi,
} from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IIssueType } from '@/common/types';

interface Props extends Partial<SelectProps> {
    issueTypeId?: string
    expectStatusId?: string
}
const SelectStatus: React.FC<Props> = (otherProps) => {
  const config = useMemo((): SelectConfig<IIssueType> => ({
    name: 'statusId',
    textField: 'name',
    valueField: 'id',
    request: ({ filter, page }) => statusApi.loadByProject('agile'),
    paging: false,
  }), []);
  const props = useSelect(config);
  return (
    <Select
      {...props}
      {...otherProps}
    />
  );
};
export default SelectStatus;
