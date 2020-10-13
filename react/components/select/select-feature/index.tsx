import React, { useMemo, forwardRef } from 'react';
import { Select, Tooltip } from 'choerodon-ui/pro';
import { featureApi } from '@/api';
import { find } from 'lodash';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import type { Issue } from '@/common/types';

interface Props {
  featureId?: string,
  featureName?: string,
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (epics: (Issue | {issueId: string, summary: string})[]) => void
}

const SelectFeature: React.FC<Props> = forwardRef(({
  dataRef, featureId, featureName, afterLoad, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<Issue> => ({
    name: 'feature',
    textField: 'summary',
    valueField: 'issueId',
    request: ({ filter, page }) => featureApi.getByEpicId(undefined, filter, page),
    // @ts-ignore
    middleWare: (features) => {
      if (featureId && featureName) {
        const moreFeatures = (find(features, (
          item,
        ) => item.issueId === featureId) || !featureId) ? features : [
            ...features, { issueId: featureId, summary: featureName }];
        if (dataRef) {
          Object.assign(dataRef, {
            current: moreFeatures,
          });
        }
        if (afterLoad) {
          afterLoad(moreFeatures);
        }
        return moreFeatures;
      }
      if (dataRef) {
        Object.assign(dataRef, {
          current: features,
        });
      }
      if (afterLoad) {
        afterLoad(features);
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
