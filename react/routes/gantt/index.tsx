import React, { useState, useEffect } from 'react';
import { Button } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import {
  Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import Gantt from '@/components/gantt';
import { ganttApi } from '@/api';
import UserHead from '@/components/UserHead';

const GanttPage: React.FC = () => {
  const [data, setData] = useState([]);
  const [columns, setColumns] = useState([{
    width: 214,
    name: 'summary',
    label: '名称',
  },
  {
    width: 100,
    name: 'assignee',
    label: '经办人',
    // @ts-ignore
    render: (record) => <UserHead user={record.assignee} />,
  },
  {
    width: 100,
    name: 'estimatedStartTime',
    label: '预计开始时间',
  },
  {
    width: 100,
    name: 'estimatedEndTime',
    label: '预计结束时间',
  }]);
  useEffect(() => {
    (async () => {
      const [headers, res] = await Promise.all([
        ganttApi.loadHeaders(),
        ganttApi.load(),
      ]);
      // setColumns(headers.map((h: any) => ({
      //   width: 100,
      //   name: h.fieldCode,
      //   label: h.name,
      // })));
      setData(res.map((r: any) => ({
        ...r,
        startDate: r.estimatedStartTime || '',
        endDate: r.estimatedEndTime || '',
      })));
    })();
  }, []);
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
        {columns.length > 0 && <Gantt data={data} columns={columns} />}
      </Content>
    </Page>
  );
};
export default observer(GanttPage);
