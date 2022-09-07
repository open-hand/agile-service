import React, { forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import useSelectPro, { FragmentForSearch } from '@/hooks/useSelectPro';
import { IIssueType } from '@/common/types';
import useProjectIssueTypes, { ProjectIssueTypesConfig } from '@/hooks/data/useProjectIssueTypes';
import TypeTag from '@/components/TypeTag';

export interface SelectIssueTypeProps extends Partial<SelectProps> {
  valueField?: string
  flat?: boolean
  projectId?: string
  config?: ProjectIssueTypesConfig
  queryOptions?: Parameters<typeof useProjectIssueTypes>[1]
  showIcon?: boolean
}

const SelectIssueType: React.FC<SelectIssueTypeProps> = forwardRef(({
  valueField, flat, projectId, config, queryOptions, showIcon = false,
  ...otherProps
}, ref: React.Ref<Select>) => {
  const { data: issueTypes } = useProjectIssueTypes({ projectId, ...config }, queryOptions);
  const [props] = useSelectPro<IIssueType>({
    textField: 'name',
    valueField: valueField || 'id',
    data: issueTypes || [],
    paging: false,
    optionRenderer: showIcon ? (issueType) => (
      <FragmentForSearch name={issueType.name}>
        <TypeTag data={issueType} showName featureType={issueType.typeCode === 'feature' ? 'business' : undefined} />
      </FragmentForSearch>
    ) : undefined,
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
