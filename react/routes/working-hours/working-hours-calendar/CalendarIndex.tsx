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
import { workingHoursApi } from '@/api';
import { getProjectId, getOrganizationId } from '@/utils/common';
import NoData from './NoData.svg';
import styles from './CalendarIndex.less';

const WorkingHoursCalendar = () => {
  const {
    loadData, exportDs, searchDs, calendarDs, loading, startTime, endTime, userIds, projectIds, workGroupIds,
  } = useCalendarStore();
  const handleOpenExport = useCallback(() => {
    exportDs.current?.set('userIds', userIds?.length ? toJS(userIds) : undefined);
    exportDs.current?.set('projectIds', projectIds?.length ? toJS(projectIds) : undefined);
    exportDs.current?.set('startTime', moment(startTime).startOf('day'));
    exportDs.current?.set('endTime', moment(endTime).endOf('day'));
    exportDs.current?.set('workGroupIds', searchDs.current?.get('workGroupIds'));
    openExportLogModal({
      exportDs,
      title: '导出工时日历',
      action: 'download_file_work_hours_calendar',
      proMessageKey: `agile-export-work-hours-calendar-${getProjectId()}`,
      orgMessageKey: `agile-export-work-hours-calendar-org-${getOrganizationId()}`,
      exportFn: (data) => workingHoursApi.exportCalendar(data),
      fileName: '工时日历',
    });
  }, [endTime, exportDs, projectIds, searchDs, startTime, userIds]);
  const refresh = useCallback(() => {
    loadData();
  }, [loadData]);

  return (
    <Page className={styles.calendarIndex}>
      <Header>
        <Search searchDs={searchDs} showWorkGroup />
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
      <Content style={{ overflowX: 'hidden' }}>
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
            calendarDs.totalPage === 0 && !(calendarDs.status === 'loading' || loading) && (
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
            )
          }
          <Calendar />
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
