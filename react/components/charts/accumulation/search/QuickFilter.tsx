import React, { useState, useEffect, useCallback } from 'react';
import { Select } from 'choerodon-ui/pro';
import { quickFilterApi } from '@/api';
import { IQuickFilter } from '@/components/quick-search';

const { Option } = Select;
interface Props {
  onChange: (filterIds: string[]) => void
  value: string[]
  projectId?: string
}
const QuickFilter: React.FC<Props> = ({
  onChange, value, projectId,
}) => {
  const [quickFilterList, setQuickFilterList] = useState<IQuickFilter[]>([]);
  const refresh = useCallback(async () => {
    const list = await quickFilterApi.project(projectId).loadAll();
    setQuickFilterList(list);
  }, []);
  useEffect(() => {
    refresh();
  }, [refresh]);
  return (
    <Select
      multiple
      value={value}
      placeholder="快速筛选"
      label="快速筛选"
      style={{ marginLeft: 15 }}
      onChange={onChange}
    >
      {quickFilterList.map((quickFilter) => (
        <Option
          value={quickFilter.filterId}
        >
          {quickFilter.name}
        </Option>
      ))}
    </Select>
  );
};
export default QuickFilter;
