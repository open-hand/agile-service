import React, { useMemo } from 'react';
import { Page, Header, Content } from '@choerodon/boot';
import {
  Button, DataSet,
} from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { IStatus } from '@/common/types';
import { ColumnProps } from 'choerodon-ui/pro/lib/table/Column';
import openSelectExistStatus from '../components/select-exist-status';
import openCreateStatus from '../components/create-status';
import openSetDefaultStatus from '../components/set-default-status';
import Table from './Table';
import { TabComponentProps } from '..';

interface IStatusCirculation extends IStatus {
  to: string[]
}
const StatusCirculation: React.FC<TabComponentProps> = ({ tab }) => {
  const statusList: IStatusCirculation[] = [{
    id: '1',
    name: '待处理',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '2',
    name: '待处理2',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '3',
    name: '待处理3',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '4',
    name: '待处理4',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '5',
    name: '待处理5',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '6',
    name: '待处理6',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '7',
    name: '待处理7',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '8',
    name: '待处理7',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '9',
    name: '待处理7',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '10',
    name: '待处理7',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '11',
    name: '待处理7',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '12',
    name: '待处理7',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '13',
    name: '待处理7',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '14',
    name: '待处理7',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '15',
    name: '待处理7',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '16',
    name: '待处理7',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '17',
    name: '待处理7',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '18',
    name: '待处理7',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '19',
    name: '待处理7',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '20',
    name: '待处理7',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '21',
    name: '待处理7',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '22',
    name: '待处理7',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }];
  const columns: ColumnProps[] = [{ name: 'name', lock: true }, { name: 'operate', lock: true }].concat(statusList.map((status) => ({
    name: status.name,
    lock: false,
    renderer: (() => <input type="checkbox" checked />),
  })));
  const data = statusList.map((from) => statusList.reduce((result, to) => ({
    ...result,
    [to.name]: from.to.includes(to.id),
    name: from.name,
    operate: '可流转到',
  }), {}));
  const dataSet = useMemo(() => new DataSet({
    primaryKey: 'id',
    name: 'status',
    autoQuery: false,
    paging: false,
    selection: false,
    data,
    fields: statusList.map((status) => ({
      name: status.name,
      type: 'boolean' as FieldType,
      label: status.name,
    })),
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
        <Button
          icon="settings"
          onClick={() => {
            openSetDefaultStatus({
              onSubmit: () => {

              },
            });
          }}
        >
          设置初始状态

        </Button>
      </Header>
      <Content>
        {tab}
        {/* <Table
          autoHeight={{
            type: 'maxHeight' as TableAutoHeightType,
            diff: 80,
          }}
          key="user"
          dataSet={dataSet}
          columns={columns}
        /> */}
        <Table data={data} columns={columns} />
      </Content>
    </Page>
  );
};

export default StatusCirculation;
