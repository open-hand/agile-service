import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { FlatSelect } from '@choerodon/components';

import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import {
  featureApi,
} from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';

interface IFeature {
  issueId: string
  summary: string
}
export interface SelectSubFeatureProps extends Partial<SelectProps> {
  featureIds?: Array<string | number>,
  flat?: boolean
  afterLoad?: (features: any[]) => void
}
/**
 * 子项目查询项目群特性
 */
const SelectSubFeature: React.FC<SelectSubFeatureProps> = forwardRef(({
  featureIds, afterLoad, flat, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<IFeature> => ({
    name: 'featureId',
    textField: 'summary',
    valueField: 'issueId',
    tooltip: true,
    request: ({ filter, page }) => featureApi.queryAllInSubProject(featureIds || [], filter!, page, 50),
    middleWare: (data) => {
      if (afterLoad) {
        afterLoad(data);
      }
      return data;
    },
  }), [afterLoad, featureIds]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      ref={ref}
      popupStyle={{ maxWidth: '3rem !important' }}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectSubFeature;
