import React, { useMemo } from 'react';
import { Page, Header, Content } from '@choerodon/boot';
import { Button, Table, DataSet } from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { statusApiConfig } from '@/api';
import { TabComponentProps } from '../index';
import openCreateStatus from '../components/create-status';

const { Column } = Table;
const Status: React.FC<TabComponentProps> = ({ tab }) => {
  const dataSet = useMemo(() => new DataSet({
    primaryKey: 'id',
    name: 'status',
    autoQuery: true,
    paging: false,
    selection: false,
    transport: {
      read: statusApiConfig.loadByProject(),
    },
    fields: [
      {
        name: 'name',
        type: 'string' as FieldType,
        label: '名称',
      },
      {
        name: 'stage',
        type: 'intl' as FieldType,
        label: '阶段',
      },
      {
        name: 'use',
        type: 'string' as FieldType,
        label: '使用情况',
      },
      {
        name: 'operate',
        type: 'boolean' as FieldType,
        label: '操作',
      },
    ],
  }), []);
  const handleCreateStatusClick = () => {
    openCreateStatus({
      onSubmit: () => {
        dataSet.query();
      },
    });
  };
  return (
    <Page>
      <Header>
        <Button icon="playlist_add" onClick={handleCreateStatusClick}>创建状态</Button>
      </Header>
      <Content>
        {tab}
        <Table key="user" dataSet={dataSet}>
          <Column name="name" />
          <Column name="stage" />
          <Column name="use" />
          <Column name="operate" />
        </Table>
      </Content>
    </Page>
  );
};
export default Status;
