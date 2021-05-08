import React from 'react';
import ColumnManageComponent from '@/components/table-cache/column-manage';
import TableCacheComponent, { TableCacheProps } from '@/components/table-cache';
import { ColumnManageProps } from './column-manage/Modal';

export default function create(type: string) {
  return {
    ColumnManage: (props: Omit<ColumnManageProps, 'type'>) => <ColumnManageComponent type={type} {...props} />,
    TableCache: (props: Omit<TableCacheProps, 'type'>) => <TableCacheComponent type={type} {...props} />,
  };
}
