import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import {
  TabPage as Page, Header, Breadcrumb, Content, HeaderButtons,
} from '@choerodon/boot';
import moment from 'moment';
import { toJS } from 'mobx';
import { LoadingProvider } from '@/components/Loading';
import LogTable from './components/LogTable';
import { openExportLogModal } from './components/export-modal';
import LogSearch from './components/LogSearch';
import { StoreProvider, useLogStore } from './stores';
import { IWorkingHoursData, workingHoursApi } from '@/api';
import { getProjectId, getOrganizationId } from '@/utils/common';

const WorkingHoursLog = () => {
  const {
    logDs, loadData, exportDs, logSearchDs,
  } = useLogStore();
  const handleOpenExport = useCallback(() => {
    const search: IWorkingHoursData = logSearchDs.current?.data as IWorkingHoursData;
    exportDs.current?.set('userIds', search.userIds?.length ? toJS(search.userIds) : undefined);
    exportDs.current?.set('projectIds', search.projectIds?.length ? toJS(search.projectIds) : undefined);
    exportDs.current?.set('startTime', moment(search.startTime).startOf('day'));
    exportDs.current?.set('endTime', moment(search.endTime).endOf('day'));
    openExportLogModal({
      exportDs,
      title: '导出工时日志',
      action: 'download_file_work_hours_log',
      proMessageKey: `agile-export-work-hours-log-${getProjectId()}`,
      orgMessageKey: `agile-export-work-hours-log-org-${getOrganizationId()}`,
      exportFn: (data) => workingHoursApi.exportLog(data),
      fileName: '工时日志',
    });
  }, [exportDs, logSearchDs]);
  const refresh = useCallback(() => {
    loadData();
  }, [loadData]);

  return (
    <Page>
      <Header>
        <LogSearch searchDs={logSearchDs} />
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

const ObserverWorkingHoursLog = observer(WorkingHoursLog);

const Index = (props: any) => (
  <StoreProvider {...props}>
    <ObserverWorkingHoursLog />
  </StoreProvider>
);

export default Index;
