import React, {
  useMemo, forwardRef, useImperativeHandle, Ref,
} from 'react';
import { Select, Tooltip } from 'choerodon-ui/pro';
import { find } from 'lodash';
import { FlatSelect } from '@choerodon/components';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { featureApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import type { Issue } from '@/common/types';

interface Props extends Partial<SelectProps> {
  featureId?: string,
  featureName?: string,
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (epics: (Issue | { issueId: string, summary: string })[]) => void
  flat?: boolean
  request?: SelectConfig<Issue>['request']
  projectId?: string
  useSelectRef?: Ref<SelectProps>
}

const SelectFeature: React.FC<Props> = forwardRef(({
  dataRef, featureId, featureName, afterLoad, request, flat, projectId, useSelectRef, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<Issue> => ({
    name: 'feature',
    textField: 'summary',
    valueField: 'issueId',
    tooltip: true,
    request: request || (({ filter, page }) => featureApi.getByEpicId(undefined, filter, page, 50, projectId)),
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
    // optionRenderer: (feature) => (
    //   <Tooltip title={feature.summary} placement="left">
    //     {feature.summary}
    //   </Tooltip>
    // ),
    paging: true,
  }), [featureId, request, featureName]);
  const props = useSelect(config);
  useImperativeHandle(useSelectRef, () => props);
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      ref={ref}
      clearButton
      popupStyle={{ maxWidth: '3rem !important' }}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectFeature;
