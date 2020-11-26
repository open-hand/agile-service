import React, { useState } from 'react';
import { Button } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import {
  Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import Gantt from '@/components/gantt';

const dataList = [
  {
    executor: null,
    content: 'SCRUM敏捷实践集',
    startDate: '2020-11-01 08:02:02',
    endDate: '2020-11-02',
    collapsed: false,
    children: [],
  },
  {
    executor: null,
    content: '风险的哈哈哈',
    startDate: null,
    endDate: null,
    collapsed: false,
    children: [
      {
        executor: null,
        content: '我的子任务',
        startDate: '2020-11-01 08:02:02',
        endDate: '2020-11-02',
        collapsed: false,
        children: [],
      },
      {
        executor: null,
        content: '我的子任务2',
        startDate: '2020-11-01 08:02:02',
        endDate: '2020-11-02',
        collapsed: false,
        children: [{
          executor: null,
          content: '我的子任务3',
          startDate: null,
          endDate: null,
          collapsed: false,
        },
        {
          executor: null,
          content: '我的子任务4',
          startDate: '2020-08-18',
          endDate: '2020-08-19',
          collapsed: false,
        }],
      },
    ],
  },
];
const dataColumns = [
  {
    width: 214,
    minWidth: 210,
    name: 'content',
    label: '名称',
    visible: true,
    keepVisible: true,
    sortable: true,
  },
  {
    width: 100,
    minWidth: 52,
    name: 'executor',
    label: '经办人',
    visible: true,
    keepVisible: false,
    sortable: true,
  },
  {
    width: 120,
    minWidth: 70,
    name: 'startDate',
    label: '预计开始',
    visible: true,
    keepVisible: false,
    sortable: true,
  },
  {
    width: 100,
    minWidth: 70,
    name: 'endDate',
    label: '预计结束',
    visible: true,
    keepVisible: false,
    sortable: false,
  },
];
const GanttPage: React.FC = () => {
  const [data, setData] = useState(dataList);
  const [columns, setColumns] = useState(dataColumns);
  return (
    <Page>
      <Header>
        <Button
          icon="playlist_add"
        >
          创建问题
        </Button>
      </Header>
      <Breadcrumb />
      <Content style={{
        borderTop: '1px solid rgb(216, 216, 216)',
        display: 'flex',
        flexDirection: 'column',
      }}
      >
        <Gantt data={data} columns={columns} />
      </Content>
    </Page>
  );
};
export default observer(GanttPage);
