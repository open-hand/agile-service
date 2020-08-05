import React, { useMemo } from 'react';
import { Page, Header, Content } from '@choerodon/boot';
import { Button, Table, DataSet } from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { TabComponentProps } from '../index';
import openSelectExistStatus from '../components/select-exist-status';
import openCreateStatus from '../components/create-status';

const { Column } = Table;
const StatusCirculation: React.FC<TabComponentProps> = ({ tab }) => {
  const dataSet = useMemo(() => new DataSet({
    primaryKey: 'id',
    name: 'status',
    autoQuery: false,
    pageSize: 10,
    selection: false,
    fields: [
      {
        name: 'name',
        type: 'string' as FieldType,
        label: '待处理',
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
  return (
    <Page>
      <Header>
        <Button
          icon="playlist_add"
          onClick={() => {
            openSelectExistStatus({
              onSubmit: () => {

              },
            });
          }}
        >
          添加已有状态
        </Button>
        <Button
          icon="playlist_add"
          onClick={() => {
            openCreateStatus({
              onSubmit: () => {

              },
            });
          }}
        >
          创建新的状态
        </Button>
        <Button icon="settings">设置初始状态</Button>
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
export default StatusCirculation;
