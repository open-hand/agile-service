import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { featureApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';

interface Props {
}

const SelectFeature: React.FC<Props> = forwardRef(({ featureId, featureName, ...otherProps }, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'feature',
    textField: 'summary',
    valueField: 'issueId',
    request: () => featureApi.getByEpicId(),
    middleWare: features => ((features.find(item => item.issueId === featureId) || !featureId) ? features : [...features, { issueId: featureId, summary: featureName }]),
    paging: false,
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
