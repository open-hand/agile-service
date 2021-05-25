import React, { forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import useSelectPro from '@/hooks/useSelectPro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IIssueType } from '@/common/types';
import { FlatSelect } from '@choerodon/components';
import useProjectIssueTypes, { ProjectIssueTypesConfig } from '@/hooks/data/useProjectIssueTypes';

interface Props extends Partial<SelectProps> {
  valueField?: string
  flat?: boolean
  config?: ProjectIssueTypesConfig
  queryOptions?: Parameters<typeof useProjectIssueTypes>[1]
}

const SelectIssueType: React.FC<Props> = forwardRef(({
  valueField, flat, config, queryOptions,
  ...otherProps
}, ref: React.Ref<Select>) => {
  const { data: issueTypes } = useProjectIssueTypes(config, queryOptions);
  const [props] = useSelectPro<IIssueType>({
    textField: 'name',
    valueField: valueField || 'id',
    data: issueTypes || [],
    paging: false,
  });
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      ref={ref}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectIssueType;
