import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { featureApi } from '@/api';
import { find } from 'lodash';
import useSelect, { SelectConfig } from '@/hooks/useSelect';

interface Props {
  featureId?: number,
  featureName?: string,
}

const SelectFeature: React.FC<Props> = forwardRef(({ featureId, featureName, ...otherProps }, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'feature',
    textField: 'summary',
    valueField: 'issueId',
    request: ({ filter, page }) => featureApi.getByEpicId(undefined, filter, page),
    middleWare: features => ((find(features, item => item.issueId === featureId) || !featureId) ? features : [...features, { issueId: featureId, summary: featureName }]),
    paging: true,
  }), []);
  const props = useSelect(config);
  return (
    <Select
      ref={ref}
      clearButton
      {...props}
      {...otherProps}
    />
  );
});
export default SelectFeature;
