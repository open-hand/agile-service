import React, { useMemo, forwardRef } from 'react';
import { Select, Tooltip } from 'choerodon-ui/pro';

import { observer } from 'mobx-react-lite';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import {
  featureApi, issueTypeApi, IStatusCirculation, statusTransformApi,
} from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';

interface IFeature {
  issueId: string
  summary: string
}
export interface SelectSubFeatureProps extends Partial<SelectProps> {
  featureIds?: number[],
  afterLoad?: (features: any[]) => void
}
/**
 * 子项目查询项目群特性
 */
const SelectSubFeature: React.FC<SelectSubFeatureProps> = forwardRef(({ featureIds, afterLoad, ...otherProps }, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<IFeature> => ({
    name: 'featureId',
    textField: 'summary',
    valueField: 'issueId',
    request: ({ filter, page }) => featureApi.queryAllInSubProject(featureIds || [], filter!, page, 10),
    middleWare: (data) => {
      if (afterLoad) {
        afterLoad(data);
      }
      return data;
    },
    optionRenderer: (feature) => (
      <Tooltip title={feature.summary}>
        <div className="text-overflow-hidden">{feature.summary}</div>
      </Tooltip>
    ),
  }), [afterLoad, featureIds]);
  const props = useSelect(config);
  return (
    <Select
      ref={ref}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectSubFeature;
