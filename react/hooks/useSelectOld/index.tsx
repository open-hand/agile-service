// 代替SelectFocusLoad
import React, {
  useMemo, useRef, useCallback,
} from 'react';
import { debounce, uniqBy } from 'lodash';
import { Button, Select } from 'choerodon-ui';
import { SelectProps } from 'choerodon-ui/lib/select';
import styles from './index.less';

export interface LoadConfig {
  filter?: string,
  nextPage?: number
}
const { Option } = Select;
export interface SelectConfig<T = {}> {
  data: T[]
  hasNextPage?: boolean
  fetchNextPage?: () => void
  onSearch?: (param: string) => void
  render: (item: T) => JSX.Element
  paging?: boolean
}

export default function useSelect<T extends { [key: string]: any }>(config: SelectConfig<T>): [SelectProps] {
  const {
    data, render, hasNextPage, fetchNextPage, paging, onSearch,
  } = config;
  // 不分页时，本地搜索
  const localSearch = !paging;
  const searchData = useMemo(() => debounce((filter: string) => {
    onSearch && onSearch(filter);
  }, 500), [onSearch]);

  // 渲染去掉重复项
  const Options = uniqBy(data.map((item) => render(item)), (option) => option.props.value);
  const loadMoreButton = useMemo(() => (
    <Option
      style={{ display: hasNextPage ? 'block' : 'none', cursor: 'pointer' }}
      key="SelectFocusLoad-loadMore"
      className="SelectFocusLoad-loadMore"
      disabled
    >
      {Options.length > 0
        ? <Button type="primary" style={{ textAlign: 'left', width: '100%', background: 'transparent' }} onClick={fetchNextPage}>更多</Button>
        : '无匹配结果'}
    </Option>
  ), [Options.length, fetchNextPage, hasNextPage]);
  const selectProps: SelectProps = {
    // @ts-ignore
    children: Options.concat(loadMoreButton),
    filter: true,
    filterOption: false,
    onFilterChange: searchData,
    dropdownClassName: 'hidden-text hidden-label minSelectFocusLoadDropDownWidth',
  };
  return [selectProps];
}
