import React, { useState, useEffect } from 'react';
import { Button } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import {
  Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import GanttComponent from '@/components/gantt';
import { ganttApi } from '@/api';
import UserHead from '@/components/UserHead';
import { Gantt } from '@/components/gantt/types';
import Search from './components/search';

function test(data: any) {
  const map = new Map<string, any>();
  const result: any[] = [];
  data.forEach((item: any) => {
    map.set(item.issueId, item);
  });
  data.forEach((item: any) => {
    if (!item.parentId) {
      result.push(item);
    } else {
      const parent = map.get(String(item.parentId));
      if (parent) {
        if (!parent.children) {
          parent.children = [];
        }
        parent.children.push(item);
      }
    }
  });
  console.log(result);
  return result;
}
const tableColumns = [{
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
}];
const GanttPage: React.FC = () => {
  const [data, setData] = useState<any[]>([]);
  const [columns, setColumns] = useState<Gantt.Column[]>([]);
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
      setColumns(tableColumns);
      // setData(test(res.map((r: any) => ({
      //   ...r,
      //   startDate: r.estimatedStartTime || '',
      //   endDate: r.estimatedEndTime || '',
      //   collapsed: false,
      // }))));
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
        <Search />
        {columns.length > 0 && <GanttComponent data={data} columns={columns} />}

      </Content>
    </Page>
  );
};
export default observer(GanttPage);
