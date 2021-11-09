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
import { getProjectId, getOrganizationId } from '@/utils/common';
import NoData from './NoData.svg';
import styles from './index.less';
import useIssueTableFields from '@/hooks/data/useIssueTableFields';
import ModeSwitch from './components/mode-switch';

const WorkingHoursIssue = () => {
  const {
    loadData, dateSearchDs, loading, workingHoursIssuesDs, mode, setMode, isProject,
  } = useIssueStore();
  const { data: tableFields } = useIssueTableFields();
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

const Index = (props: any) => (
  <StoreProvider {...props}>
    <ObserverWorkingHoursIssue />
  </StoreProvider>
);

export default Index;
