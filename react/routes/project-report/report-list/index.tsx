import React, { useCallback } from 'react';
import {
  Page, Header, Breadcrumb, Content,
} from '@choerodon/boot';
import { Button } from 'choerodon-ui/pro';
import to from '@/utils/to';
import ReportTable from './components/report-table';

const ReportList: React.FC = () => {
  const handleAddClick = useCallback(() => {
    to('/agile/project-report/create');
  }, []);
  return (
    <Page>
      <Header>
        <Button icon="playlist_add" onClick={handleAddClick}>添加报告内容</Button>
      </Header>
      <Breadcrumb />
      <Content style={{ paddingTop: 0 }}>
        <ReportTable onClick={handleAddClick} />
      </Content>
    </Page>
  );
};

export default ReportList;
