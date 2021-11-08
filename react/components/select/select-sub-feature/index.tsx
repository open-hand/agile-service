import React, { useMemo, forwardRef, useRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { FlatSelect } from '@choerodon/components';

import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { useComputed } from 'mobx-react-lite';
import { uniq } from 'lodash';
import {
  featureApi,
} from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { useNoticeSelectUpdateSelected } from '../useNoticeSelectUpdateSelected';
import { refsBindRef, wrapRequestCallback } from '../utils';

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
  featureIds: propsFeatureIds, afterLoad, flat, ...otherProps
}, ref: React.Ref<Select>) => {
  const selectRef = useRef<Select>();
  const selectIdsRef = useRef<Array<string | number>>(propsFeatureIds || []);
  const values = useComputed(() => (selectRef.current?.getValues() || []).flat(Infinity).map((item) => (typeof item === 'object' ? item.issueId : item)), [selectRef.current?.getValues()]);
  const optionsRef = useRef<any[]>();
  const featureIds = useMemo(() => uniq([...values, ...(propsFeatureIds || [])]).filter(Boolean), [values, propsFeatureIds]);
  const [forceValue, setFilterWord] = useNoticeSelectUpdateSelected();
  const args = useMemo(() => {
    if (optionsRef.current && featureIds) {
      // 有新的未加载的值，就重新加载
      const hasNewUnExistValue = featureIds.some((v) => !optionsRef.current?.find((item) => item.issueId === v));
      if (hasNewUnExistValue || forceValue) {
        selectIdsRef.current = featureIds;
      }
    }
    return { featureIds: selectIdsRef.current };
  }, [featureIds, forceValue]);
  const config = useMemo((): SelectConfig<IFeature> => ({
    name: 'featureId',
    textField: 'summary',
    valueField: 'issueId',
    tooltip: true,
    requestArgs: args,
    request: wrapRequestCallback(
      ({ filter, page, requestArgs }) => featureApi.queryAllInSubProject(requestArgs?.featureIds || [], filter!, page, 50),
      ({ filter }) => setFilterWord('filter', filter),
    ),
    middleWare: (data) => {
      if (afterLoad) {
        afterLoad(data);
      }
      optionsRef.current = data;
      return data;
    },
  }), [afterLoad, args]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      ref={refsBindRef(ref, selectRef)}
      popupStyle={{ maxWidth: '3rem !important' }}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectSubFeature;
