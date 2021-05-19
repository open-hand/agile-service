import React, { useCallback } from 'react';
import { Select, Tooltip } from 'choerodon-ui';
import { find } from 'lodash';
import {
  useSetState, useLockFn, useDebounceFn, useMount,
} from 'ahooks';
import Tip from '@/components/Tip';
import { devOpsApi } from '@/api';

const { Option, OptGroup } = Select;

const SelectApp = ({ onChange, onAppChange, ...props }) => {
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
    setState((s) => ({
      page: s.page + 1,
      data: s.data.concat(list),
      loading: false,
      hasNextPage,
    }));
  });
  const { run: handleSearch, flush } = useDebounceFn(async (input, force) => {
    if (input === state.search && !force) {
      return;
    }
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
  useMount(() => {
    handleSearch('', true);
    flush();
  });
  const handleChange = useCallback((appId) => {
    const { projectId } = find(state.data, (project) => project.appServices.find((s) => String(s.id) === String(appId)));
    onChange(appId);
    onAppChange(appId, String(projectId));
  }, [onAppChange, onChange, state.data]);
  const { data, loading, hasNextPage } = state;
  return (
    <div style={{ position: 'relative' }}>
      <Select
        label="应用服务"
        allowClear
        filter
        filterOption={() => true}
        loading={loading}
        onFilterChange={handleSearch}
        onChange={handleChange}
        {...props}
      >
        {data.map((project) => (
          <OptGroup label={project.projectName} key={project.projectName}>
            {project.appServices.map((s) => (
              <Option value={s.id} key={s.id}>
                <Tooltip title={s.code}>
                  {`${s.name}(${s.code})`}
                </Tooltip>
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
              color: '#5365EA',
            }}
            onClick={handleLoadMore}
          >
            查看更多
          </div>
        </Option>
        )}
      </Select>
      <div style={{ position: 'absolute', right: -25, top: 7 }}>
        <Tip title="此处会展示出每个项目下最多5个应用服务，若想选择其他应用服务，需要手动输入服务名进行搜索" />
      </div>
    </div>

  );
};
export default SelectApp;
