import React from 'react';
import { Select } from 'choerodon-ui';
import { useSetState, useLockFn, useDebounceFn } from 'ahooks';
import { devOpsApi } from '@/api';

const { Option, OptGroup } = Select;


const SelectApp = (props) => {
  const [state, setState] = useSetState({
    loading: false,
    data: [],
    page: 1,
    size: 10,
    hasNextPage: false,
    search: '',
  });
  const handleLoadMore = useLockFn(async (e) => {
    e.stopPropagation();
    setState({
      loading: true,
    });
    const { list, hasNextPage } = await devOpsApi.loadProjectActiveService(state.page + 1, state.size, state.search);
    setState(s => ({
      page: s.page + 1,
      data: s.data.concat(list),
      loading: false,
      hasNextPage,
    }));
  });
  const { run: handleSearch } = useDebounceFn(async (input) => {
    setState({
      loading: true,
      search: input,
    });
    const { list, hasNextPage } = await devOpsApi.loadProjectActiveService(1, state.size, input);
    setState({
      data: list,
      page: 1,
      loading: false,
      hasNextPage,
    });
  });
  const { data, loading, hasNextPage } = state;
  return (
    <Select
      label="应用服务"
      allowClear
      filter
      filterOption={() => true}
      loading={loading}
      onFilterChange={handleSearch}
      {...props}
    >
      {data.map(project => (
        <OptGroup label={project.projectName} key={project.projectName}>
          {project.appServices.map(s => (
            <Option value={s.id} key={s.id}>
              {s.name}
            </Option>
          ))}
        </OptGroup>
      ))}
      {hasNextPage && (
        <Option key="more">
          <div
            role="none"
            style={{
              margin: '-4px -20px',
              padding: '4px 20px',
              color: '#3f51b5',
            }}
            onClick={handleLoadMore}
          >
            查看更多
          </div>
        </Option>
      )}
    </Select>
  );
};
export default SelectApp;
