import React, {
  forwardRef, useState, useEffect,
} from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { useCreation } from 'ahooks';
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
  afterLoad?: (piList: PI[]) => void
  disabledRequest?: boolean
  /** 常用选项  @default SelectQuickFilterDefaultCommonOptions */
  commonOptions?: Array<SelectQuickFilterCommonOption> | ((options: SelectQuickFilterDefaultCommonOptions) => Array<SelectQuickFilterCommonOption>)
}

const { Option, OptGroup } = Select;
const SelectQuickFilterField: React.FC<SelectQuickFilterFieldProps> = forwardRef(({
  disabledRequest = false, afterLoad, commonOptions: propsOptions = DefaultOptions, ...otherProps
}, ref: React.Ref<Select>) => {
  const [data, setData] = useState<Array<{ filterId: string, name: string }>>([]);
  const loadData = async () => {
    const quickFilterData = await quickFilterApi.loadAll({ contents: [], filterName: '' });
    setData(quickFilterData);
  };
  const commonOptions = useCreation(() => {
    if (typeof propsOptions === 'function') {
      return propsOptions(DefaultOptions) || [];
    }
    return propsOptions;
  }, []);
  useEffect(() => {
    !disabledRequest && loadData();
  }, [disabledRequest]);
  return (
    <Select
      ref={ref}
      // {...props}
      {...otherProps}
    >
      <OptGroup label="常用选项">
        {commonOptions.map((item) => <Option value={item.value}>{item.meaning}</Option>)}
      </OptGroup>
      {data.length > 0
        && (
          <OptGroup label="快速筛选">
            {data.map((item) => <Option value={item.filterId}>{item.name}</Option>)}
          </OptGroup>
        )}

    </Select>
  );
});
export default SelectQuickFilterField;
