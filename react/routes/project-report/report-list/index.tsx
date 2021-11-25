import React, { useCallback } from 'react';
import {
  Page, Header, Breadcrumb, Content,
} from '@choerodon/boot';
import { HeaderButtons } from '@choerodon/master';
import to from '@/utils/to';
import ReportTable from './components/report-table';
import useFormatMessage from '@/hooks/useFormatMessage';

const ReportList: React.FC = () => {
  const formatMessage = useFormatMessage('agile.projectReport');
  const handleAddClick = useCallback(() => {
    to('/agile/project-report/create');
  }, []);
  return (
    <Page>
      <Header>
        <HeaderButtons items={[{
          name: formatMessage({ id: 'create' }),
          handler: handleAddClick,
          icon: 'playlist_add',
          display: true,
        }]}
        />
      </Header>
      <Breadcrumb />
      <Content>
        <ReportTable onClick={handleAddClick} />
      </Content>
    </Page>
  );
};

export default ReportList;
