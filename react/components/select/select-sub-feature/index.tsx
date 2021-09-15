import React, { useMemo, forwardRef, useRef } from 'react';
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
  const selectIdsRef = useRef<Array<string | number>>(featureIds || []);
  const optionsRef = useRef<any[]>();
  const args = useMemo(() => {
    if (optionsRef.current && featureIds) {
      // 有新的未加载的值，就重新加载
      const hasNewUnExistValue = featureIds.some((v) => !optionsRef.current?.find((item) => item.issueId === v));
      if (hasNewUnExistValue) {
        selectIdsRef.current = featureIds;
      }
    }
    return { featureIds: selectIdsRef.current };
  }, [featureIds]);
  const config = useMemo((): SelectConfig<IFeature> => ({
    name: 'featureId',
    textField: 'summary',
    valueField: 'issueId',
    tooltip: true,
    requestArgs: args,
    request: ({ filter, page, requestArgs }) => featureApi.queryAllInSubProject(requestArgs?.featureIds || [], filter!, page, 50),
    middleWare: (data) => {
      if (afterLoad) {
        afterLoad(data);
      }
      return data;
    },
  }), [afterLoad, args]);
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
