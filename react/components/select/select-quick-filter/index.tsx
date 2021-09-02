import React, {
  forwardRef, useState, useEffect,
  useCallback,
} from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { useCreation, usePersistFn } from 'ahooks';
import { FlatSelect } from '@choerodon/components';
import { noop } from 'lodash';
import { quickFilterApi } from '@/api';
import type { PI } from '@/common/types';

export type SelectQuickFilterDefaultCommonOptions = [
  { value: 'myStarBeacon', meaning: '我的关注' },
  { value: 'myAssigned', meaning: '我经手的' },
]
const DefaultOptions: SelectQuickFilterDefaultCommonOptions = [{ value: 'myStarBeacon', meaning: '我的关注' },
  { value: 'myAssigned', meaning: '我经手的' }];
interface SelectQuickFilterCommonOption {
  value: string
  meaning: string
}
export interface SelectQuickFilterFieldProps extends Partial<SelectProps> {
  afterLoad?: (piList: { filterId: string, name: string }) => Array<{ filterId: string, name: string }> | void
  projectId?: string
  disabledRequest?: boolean
  flat?: boolean
  optionFlat?: boolean
  /** 常用选项 false 关闭常用选项  @default SelectQuickFilterDefaultCommonOptions */
  commonOptions?: false | Array<SelectQuickFilterCommonOption> | ((options: SelectQuickFilterDefaultCommonOptions) => Array<SelectQuickFilterCommonOption>)
}

const SelectQuickFilterField: React.FC<SelectQuickFilterFieldProps> = forwardRef(({
  disabledRequest = false, afterLoad: propsAfterLoad, commonOptions: propsOptions = DefaultOptions, flat, optionFlat, projectId, ...otherProps
}, ref: React.Ref<Select>) => {
  const afterLoad = usePersistFn(propsAfterLoad || noop);
  const [data, setData] = useState<Array<{ filterId: string, name: string }>>([]);
  const loadData = async () => {
    const quickFilterData = await quickFilterApi.project(projectId).loadAll({ contents: [], filterName: '' });
    const temp = afterLoad(quickFilterData);
    setData(Array.isArray(temp) ? temp : quickFilterData);
  };
  const commonOptions = useCreation(() => {
    if (typeof propsOptions === 'function') {
      return propsOptions(DefaultOptions) || [];
    }
    if (propsOptions !== undefined) {
      return Array.isArray(propsOptions) ? propsOptions : [];
    }
    return propsOptions;
  }, []);
  useEffect(() => {
    !disabledRequest && loadData();
  }, [disabledRequest]);
  const Component = flat ? FlatSelect : Select;
  const { Option, OptGroup } = Component;
  const renderFilterOptions = useCallback(() => {
    if (optionFlat) {
      return data.map((item) => <Option value={item.filterId}>{item.name}</Option>);
    }
    return data.length > 0
    && (
      <OptGroup label="快速筛选">
        {data.map((item) => <Option value={item.filterId}>{item.name}</Option>)}
      </OptGroup>
    );
  }, [data, optionFlat]);
  return (
    <Component
      ref={ref}
      // {...props}
      {...otherProps}
    >
      {commonOptions.length !== 0 && (
        <OptGroup label="常用选项">
          {commonOptions.map((item) => <Option value={item.value}>{item.meaning}</Option>)}
        </OptGroup>
      )}
      {renderFilterOptions()}
    </Component>
  );
});
export default SelectQuickFilterField;
