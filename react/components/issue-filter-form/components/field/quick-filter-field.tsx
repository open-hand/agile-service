import React, {
  useMemo, forwardRef, useRef, useState, useEffect,
} from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { quickFilterApi } from '@/api';
import type { PI } from '@/common/types';

interface Props extends Partial<SelectProps> {
  afterLoad?: (piList: PI[]) => void
  disabledRequest?: boolean
}
interface Filter {
  filterId: string,
  name: string,
}
const { Option, OptGroup } = Select;
const QuickFilterField: React.FC<Props> = ({
  disabledRequest = false, afterLoad, ...otherProps
}) => {
  const [data, setData] = useState<Array<{ filterId: string, name: string }>>([]);
  const loadData = async () => {
    const quickFilterData = await quickFilterApi.loadAll({ contents: [], filterName: '' });
    setData(quickFilterData);
  };

  useEffect(() => {
    !disabledRequest && loadData();
  }, [disabledRequest]);
  return (
    <Select
      // {...props}
      {...otherProps}
    >
      <OptGroup label="常用选项">
        <Option value="myStarBeacon">我的关注</Option>
        <Option value="myAssigned">我经手的</Option>
      </OptGroup>
      {data.length > 0
        && (
          <OptGroup label="快速筛选">
            {data.map((item) => <Option value={item.filterId}>{item.name}</Option>)}
          </OptGroup>
        )}

    </Select>
  );
};
export default QuickFilterField;
