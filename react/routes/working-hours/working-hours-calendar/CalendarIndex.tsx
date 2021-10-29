import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import {
  TabPage as Page, Header, Breadcrumb, Content, HeaderButtons,
} from '@choerodon/boot';
import moment from 'moment';
import { toJS } from 'mobx';
import { EmptyPage } from '@choerodon/components';
import { LoadingProvider } from '@/components/Loading';
import Calendar from './components/Calendar';
import { openExportLogModal } from '../working-hours-log/components/export-modal';
import Search from '../working-hours-log/components/LogSearch';
import { StoreProvider, useCalendarStore } from './stores';
import { IWorkingHoursData, workingHoursApi } from '@/api';
import { getProjectId, getOrganizationId } from '@/utils/common';
import NoData from './NoData.svg';

const WorkingHoursCalendar = () => {
  const {
    loadData, exportDs, searchDs, calendarDs, loading,
  } = useCalendarStore();
  const handleOpenExport = useCallback(() => {
    const search: IWorkingHoursData = searchDs.current?.data as IWorkingHoursData;
    exportDs.current?.set('userIds', search.userIds?.length ? toJS(search.userIds) : undefined);
    exportDs.current?.set('projectIds', search.projectIds?.length ? toJS(search.projectIds) : undefined);
    exportDs.current?.set('startTime', moment(search.startTime).startOf('day'));
    exportDs.current?.set('endTime', moment(search.endTime).endOf('day'));
    openExportLogModal({
      exportDs,
      title: '导出工时日历',
      action: 'download_file_work_hours_calendar',
      proMessageKey: `agile-export-work-hours-calendar-${getProjectId()}`,
      orgMessageKey: `agile-export-work-hours-calendar-org-${getOrganizationId()}`,
      exportFn: (data) => workingHoursApi.exportCalendar(data),
      fileName: '工时日历',
    });
  }, [exportDs, searchDs]);
  const refresh = useCallback(() => {
    loadData();
  }, [loadData]);

  return (
    <Page>
      <Header>
        <Search searchDs={searchDs} />
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
      <Content style={{ padding: 0, overflowX: 'hidden' }}>
        <LoadingProvider
          loading={calendarDs.status === 'loading' || loading}
          globalSingle
          style={{
            height: '100%',
            width: '100%',
            zIndex: 'auto',
          }}
        >
          {
            calendarDs.totalPage === 0 && !(calendarDs.status === 'loading' || loading) ? (
              <EmptyPage
                image={NoData}
                description="暂无数据"
                style={{
                  height: '100%',
                  width: '100%',
                  display: 'flex',
                  justifyContent: 'center',
                  paddingTop: 0,
                }}
              />
            ) : (
              <Calendar />
            )
          }
        </LoadingProvider>
      </Content>
    </Page>
  );
};

const ObserverWorkingHoursCalendar = observer(WorkingHoursCalendar);

const Index = (props: any) => (
  <StoreProvider {...props}>
    <ObserverWorkingHoursCalendar />
  </StoreProvider>
);

export default Index;
