import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { featureApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IIssueType } from '@/common/types';

interface Props extends Partial<SelectProps> {
  featureIds?: number[],
}
const FeatureProjectField: React.FC<Props> = forwardRef(({ featureIds, ...otherProps }, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<IIssueType> => ({
    name: 'featureId',
    textField: 'summary',
    valueField: 'issueId',
    request: ({ filter, page }) => featureApi.queryAllInSubProject(featureIds || [], filter!, page, 10),
  }), []);
  const props = useSelect(config);
  return (
    <Select
      ref={ref}
      {...props}
      {...otherProps}
    />
  );
});
export default FeatureProjectField;
