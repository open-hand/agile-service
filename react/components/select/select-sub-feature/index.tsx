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
import { refsBindRef } from '../utils';
import useSelectRequestArgsValue from '../useSelectRequestArgsValue';

interface IFeature {
  issueId: string
  summary: string
}
export interface SelectSubFeatureProps extends Partial<SelectProps> {
  featureIds?: Array<string | number>,
  flat?: boolean
  projectId?: string
  afterLoad?: (features: any[]) => void
}
/**
 * 子项目查询项目群特性
 */
const SelectSubFeature: React.FC<SelectSubFeatureProps> = forwardRef(({
  featureIds: propsFeatureIds, afterLoad, flat, projectId, ...otherProps
}, ref: React.Ref<Select>) => {
  const dataRef = useRef<any>();
  const selectRef = useRef<Select>();
  const values = useComputed(() => (selectRef.current?.getValues() || []).flat(Infinity).map((item) => (typeof item === 'object' ? item.issueId : item)), [selectRef.current?.getValues()]);
  const featureIds = useMemo(() => uniq([...values, ...(propsFeatureIds || [])]).filter(Boolean), [values, propsFeatureIds]);

  const selected = useSelectRequestArgsValue({ dataRef, value: featureIds });
  const args = useMemo(() => ({ featureIds: selected }), [selected]);
  const config = useMemo((): SelectConfig<IFeature> => ({
    name: 'featureId',
    textField: 'summary',
    valueField: 'issueId',
    tooltip: true,
    requestArgs: args,
    request: ({ filter, page, requestArgs }) => featureApi.project(projectId)
      .queryAllInSubProject(requestArgs?.featureIds || [], filter!, page, 50),
    afterLoad,
  }), [afterLoad, args, projectId]);
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
