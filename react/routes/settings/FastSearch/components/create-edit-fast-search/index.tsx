import React, { memo, useEffect, useMemo } from 'react';
import {
  Table, DataSet, Form, Select, TextField, Modal, Button, Col, Row,
} from 'choerodon-ui/pro';
import { quickFilterApi } from '@/api';
import FastSearch from '../fast-search';
import { transformDataToEditData } from '../fast-search/utils';

export const openCreateFastSearch = () => {
  const prefixCls = 'c7n-agile-fast-search-modal';
  Modal.open({
    key: Modal.key(),
    title: '创建快速筛选',
    style: {
      width: 740,
    },
    className: prefixCls,
    drawer: true,
    okText: '创建',
    cancelText: '取消',
    children: (
      <FastSearch />
    ),
  });
};
export const openEditFastSearch = async (id: string) => {
  const prefixCls = 'c7n-agile-fast-search-modal';
  const data = await quickFilterApi.load(id).then((res: any) => transformDataToEditData(res));
  console.log('edit...', data);
  Modal.open({
    key: Modal.key(),
    title: '修改快速筛选',
    style: {
      width: 740,
    },
    className: prefixCls,
    drawer: true,
    okText: '修改',
    cancelText: '取消',
    children: (
      <FastSearch data={data} />
    ),
  });
};
