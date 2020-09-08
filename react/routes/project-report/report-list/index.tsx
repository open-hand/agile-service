import React from 'react';
import {
  Page, Header, Breadcrumb, Content,
} from '@choerodon/boot';
import { Button } from 'choerodon-ui/pro';
import ReportTable from './components/report-table';

const ReportList: React.FC = () => (
  <Page>
    <Header>
      <Button icon="playlist_add">添加报告内容</Button>
    </Header>
    <Breadcrumb />
    <Content>
      <ReportTable />
    </Content>
  </Page>
);

export default ReportList;
