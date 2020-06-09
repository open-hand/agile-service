import React, { useState, useMemo, useEffect } from 'react';
import { Select } from 'choerodon-ui/pro';
import { debounce } from 'lodash';
import types, { TypeConfig, SelectType } from './types';

const { Option } = Select;

export { SelectType };
export default function useSelect(type: SelectType) {
  const [data, setData] = useState([]);
  const config = types.get(type);
  if (!config) {
    throw new Error('没有找到配置');
  }
  const defaultRender = (item: any) => <Option value={item[valueField]}>{item[textField]}</Option>;
  const {
    textField = 'name',
    valueField = 'id',
    render = defaultRender,
    request,
    paging = true,
    props,
  } = config as TypeConfig;
  async function loadData(search: string) {
    const res = await request(search);
    setData(paging ? res.list : res);
  }
  const searchData = useMemo(() => debounce((search: string) => {
    loadData(search);
  }, 500), []);
  useEffect(() => {
    loadData('');
  }, []);
  const handleInput = (e: React.ChangeEvent<HTMLInputElement>) => {
    const { value } = e.target;
    searchData(value);
  };

  const selectProps = {
    searchable: true,
    onInput: handleInput,
    children: data.map(render),
    ...props,
  };
  return selectProps;
}
