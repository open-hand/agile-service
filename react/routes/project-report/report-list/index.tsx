import React, { useCallback } from 'react';
import {
  Page, Header, Breadcrumb, Content,
} from '@choerodon/boot';
import { HeaderButtons } from '@choerodon/master';
import to from '@/utils/to';
import ReportTable from './components/report-table';

const ReportList: React.FC = () => {
  const handleAddClick = useCallback(() => {
    to('/agile/project-report/create');
  }, []);
  return (
    <Page>
      <Header>
        <HeaderButtons items={[{
          name: '创建报告',
          handler: handleAddClick,
          icon: 'playlist_add',
          display: true,
        }]}
        />
      </Header>
      <Breadcrumb />
      <Content style={{ paddingTop: 0 }}>
        <ReportTable onClick={handleAddClick} />
      </Content>
    </Page>
  );
};

export default ReportList;
