import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import {
  TabPage as Page, Header, Breadcrumb, Content, HeaderButtons,
} from '@choerodon/boot';
import moment from 'moment';
import { toJS } from 'mobx';
import { EmptyPage } from '@choerodon/components';
import { get } from 'lodash';
import { LoadingProvider } from '@/components/Loading';
import IssueTable from './components/issue-table';
import DateSearch from './components/DateSearch';
import DetailContainer, { useDetail } from '@/components/detail-container';
import { StoreProvider, useIssueStore } from './stores';
import NoData from './NoData.svg';
import styles from './index.less';
import useIssueTableFields from '@/hooks/data/useIssueTableFields';
import ModeSwitch from './components/mode-switch';
import openExportWorkModal from './components/export';
import useDefaultMyFilter from '@/hooks/useDefaultMyFilter';
import WorkingHoursIssueSearch from './components/search';
import { IFoundationHeader } from '@/common/types';

const WorkingHoursIssue = () => {
  const {
    loadData, dateSearchDs, loading, workingHoursIssuesDs, mode, setMode, isProject,
  } = useIssueStore();
  const { data: tableFields } = useIssueTableFields({
    extraFields: [{ code: 'workTime', title: '工时' } as IFoundationHeader,
      { code: 'historyWorkTime', title: '历史累计工时' } as IFoundationHeader,
      { code: 'estimatedWorkTime', title: '预估总工时' } as IFoundationHeader,
      { code: 'rate', title: '偏差率' } as IFoundationHeader,
    ],
  });
  const [issueDetailProps] = useDetail();
  const handleSummaryClick = useCallback((record) => {
    issueDetailProps?.open({
      path: 'issue',
      props: {
        issueId: get(record, 'issueId'),
        projectId: get(record, 'projectId'),
        applyType: 'agile',
        disabled: !isProject,
      },
      events: {
        update: () => {
          // refresh();
        },
      },
    });
  }, [isProject, issueDetailProps]);

  return (
    <Page className={styles.calendarIndex}>
      <Header>
        <DateSearch dateSearchDs={dateSearchDs} />
        <HeaderButtons items={[
          {
            display: true,
            name: '导出',
            icon: 'unarchive-o',
            handler: openExportWorkModal,
          },
          {
            display: true,
            element: <ModeSwitch />,
          },
        ]}
        />
      </Header>
      <Breadcrumb />
      <Content style={{ overflowX: 'hidden' }}>
        <LoadingProvider
          loading={false}
          globalSingle
          style={{
            height: '100%',
            width: '100%',
            zIndex: 'auto',
          }}
        >
          <div>
            <WorkingHoursIssueSearch loadData={() => {}} />
          </div>
          <IssueTable
            dataSet={workingHoursIssuesDs}
            fields={tableFields || []}
            onSummaryClick={handleSummaryClick}
          />
          <DetailContainer {...issueDetailProps} />
        </LoadingProvider>
      </Content>
    </Page>
  );
};

const ObserverWorkingHoursIssue = observer(WorkingHoursIssue);

const Index = (props: any) => {
  const { data: myFilter, isLoading } = useDefaultMyFilter();

  return !isLoading && (
    <StoreProvider {...props} myDefaultFilter={myFilter}>
      <ObserverWorkingHoursIssue />
    </StoreProvider>
  );
};

export default Index;
