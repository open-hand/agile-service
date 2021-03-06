import React from 'react';
import { Modal } from 'choerodon-ui/pro';
import { quickFilterApi } from '@/api';
import FastSearch from '../fast-search';
import { transformDataToEditData } from '../fast-search/utils';

export const openCreateFastSearch = (onOK: () => void) => {
  Modal.open({
    key: Modal.key(),
    title: '创建快速筛选',
    style: {
      width: 740,
    },
    drawer: true,
    okText: '创建',
    cancelText: '取消',
    children: (
      <FastSearch onOK={onOK} />
    ),
  });
};
export const openEditFastSearch = async (id: string, onOK: () => void) => {
  const data = await quickFilterApi.load(id).then((res: any) => transformDataToEditData(res));
  Modal.open({
    key: Modal.key(),
    title: '修改快速筛选',
    style: {
      width: 740,
    },
    drawer: true,
    okText: '修改',
    cancelText: '取消',
    children: (
      <FastSearch data={data} onOK={onOK} />
    ),
  });
};
