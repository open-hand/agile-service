import React, { useMemo, forwardRef } from 'react';
import { Select, Tooltip } from 'choerodon-ui/pro';
import { featureApi } from '@/api';
import { find } from 'lodash';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import type { Issue } from '@/common/types';

interface Props {
  featureId?: string,
  featureName?: string,
}

const SelectFeature: React.FC<Props> = forwardRef(({
  featureId, featureName, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<Issue> => ({
    name: 'feature',
    textField: 'summary',
    valueField: 'issueId',
    request: ({ filter, page }) => featureApi.getByEpicId(undefined, filter, page),
    middleWare: (features) => {
      if (featureId && featureName) {
        return (find(features, (
          item,
        ) => item.issueId === featureId) || !featureId) ? features : [
            ...features, { issueId: featureId, summary: featureName }];
      }
      return features;
    },
    optionRenderer: (feature) => (
      <Tooltip title={feature.summary} placement="left">
        {feature.summary}
      </Tooltip>
    ),
    paging: true,
  }), [featureId, featureName]);
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
