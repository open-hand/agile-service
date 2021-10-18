import React, { useCallback, useEffect, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import {
  TabPage as Page, Header, Breadcrumb, Content, HeaderButtons,
} from '@choerodon/boot';
import { LoadingProvider } from '@/components/Loading';
import LogTable from './components/LogTable';
import { openExportLogModal } from './components/export-modal';
import LogSearch from './components/LogSearch';
import { useLogStore } from './stores';

const WorkingHoursLog = () => {
  const { logDs, loadData } = useLogStore();
  const handleOpenExport = useCallback(() => {
    openExportLogModal({});
  }, []);
  const refresh = useCallback(() => {
    loadData();
  }, [loadData]);

  return (
    <Page>
      <Header>
        <LogSearch />
        <HeaderButtons items={[{
          name: '导出',
          icon: 'unarchive-o',
          handler: handleOpenExport,
          display: true,
        }, {
          display: true,
          icon: 'refresh',
          handler: refresh,
        }]}
        />
      </Header>
      <Breadcrumb />
      <Content style={{ borderTop: '1px solid var(--divider)' }}>
        <LoadingProvider
          loading={logDs.status === 'loading'}
          globalSingle
          style={{
            height: '100%',
            width: '100%',
            zIndex: 'auto',
            marginTop: -16,
          }}
        >
          <LogTable />
        </LoadingProvider>
      </Content>
    </Page>
  );
};

export default observer(WorkingHoursLog);
