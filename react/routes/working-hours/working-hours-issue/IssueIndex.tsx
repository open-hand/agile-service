import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import {
  TabPage as Page, Header, Breadcrumb, Content, HeaderButtons,
} from '@choerodon/boot';
import { CheckBox } from 'choerodon-ui/pro';
import { LoadingProvider } from '@/components/Loading';
import IssueTable from './components/issue-table';
import AssigneeIssueTable from './components/assignee-issue-table';
import DateSearch from './components/DateSearch';
import { StoreProvider, useIssueStore } from './stores';
import styles from './index.less';
import ModeSwitch from './components/mode-switch';
import openExportWorkModal from './components/export';
import useDefaultMyFilter from '@/hooks/useDefaultMyFilter';
import WorkingHoursIssueSearch from './components/search';

const WorkingHoursIssue = () => {
  const {
    loadData, dateSearchDs, loading, workingHoursIssuesDs, workingHoursAssigneeDs, mode, isProject, tableFields, isContain, setIsContain,
  } = useIssueStore();

  const handleChangeIsContain = useCallback((value) => {
    setIsContain(value);
  }, [setIsContain]);

  return (
    <Page className={styles.calendarIndex}>
      <Header>
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
          {
            display: true,
            element: <DateSearch dateSearchDs={dateSearchDs} />,
          },
          {
            display: true,
            element: <CheckBox style={{ marginLeft: 16 }} name="hiddenColumn" checked={isContain} onChange={handleChangeIsContain}>父级任务工时统计是否包含子任务</CheckBox>,
          },
        ]}
        />
      </Header>
      <Breadcrumb />
      <Content style={{ overflowX: 'hidden' }}>
        <LoadingProvider
          loading={loading}
          globalSingle
          style={{
            zIndex: 'auto',
            height: '100%',
            overflow: 'auto',
          }}
        >
          <div>
            <WorkingHoursIssueSearch loadData={() => {}} />
          </div>
          {
            mode === 'issue' && (
            <IssueTable
              dataSet={workingHoursIssuesDs}
            />
            )
          }
          {
            mode === 'assignee' && isProject && (
              <AssigneeIssueTable />
            )
          }
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
