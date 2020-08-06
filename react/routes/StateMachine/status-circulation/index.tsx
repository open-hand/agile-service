import React, { ReactNode } from 'react';
import { Page, Header, Content } from '@choerodon/boot';
import { Button } from 'choerodon-ui/pro';
import { IStatus } from '@/common/types';
import openSelectExistStatus from '../components/select-exist-status';
import openCreateStatus from '../components/create-status';
import openSetDefaultStatus from '../components/set-default-status';
import Table from './Table';
import { TabComponentProps } from '..';

interface ColumnProps {
  name: string,
  lock?: boolean | 'right',
  renderHeader?: () => ReactNode | null,
  renderer?: ((record: Object) => ReactNode),
}
interface IStatusCirculation extends IStatus {
  to: string[];
  [propName: string]: any
}
const StatusCirculation: React.FC<TabComponentProps> = ({ tab }) => {
  const statusList: IStatusCirculation[] = [{
    id: '1',
    name: '待处理',
    valueCode: 'todo',
    to: ['1', '2'],
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
    name: '待处理8',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '9',
    name: '待处理9',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '10',
    name: '待处理10',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '11',
    name: '待处理11',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '12',
    name: '待处理12',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '13',
    name: '待处理13',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '14',
    name: '待处理14',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '15',
    name: '待处理15',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '16',
    name: '待处理16',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '17',
    name: '待处理17',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '18',
    name: '待处理18',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '19',
    name: '待处理19',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '20',
    name: '待处理20',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '21',
    name: '待处理21',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }, {
    id: '22',
    name: '待处理22',
    valueCode: 'todo',
    to: ['1', '2', '3'],
  }];
  const data = statusList.map((from) => statusList.reduce((result, to) => ({
    ...result,
    [to.id]: from.to.includes(to.id),
    name: from.name,
  }), {}));
  const columns: ColumnProps[] = [{
    name: 'name',
    lock: true,
    renderHeader: () => null,
  }, {
    name: 'operate',
    lock: true,
    renderHeader: () => null,
    renderer: (() => '可流转到'),
  },
  ...statusList.map((status) => ({
    name: status.name,
    renderer: ((record: IStatusCirculation) => <input type="checkbox" checked={record[status.id]} />),
  })),
  {
    name: 'delete',
    lock: 'right',
    renderHeader: () => null,
    renderer: (() => 'delete'),
  }];

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
        <Table data={data} columns={columns} />
      </Content>
    </Page>
  );
};

export default StatusCirculation;
