import React, { useCallback, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import {
  TabPage as Page, Header, Breadcrumb, Content, HeaderButtons,
} from '@choerodon/boot';
import { CheckBox, Button } from 'choerodon-ui/pro';
import { Icon } from 'choerodon-ui';
import { includes } from 'lodash';
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
import { openCustomColumnManageModal } from '@/components/table-cache/column-manage/Modal';
import { getProjectId, getMenuType } from '@/utils/common';
import getColumnManageOptions from './utils/getColumnManageOptions';
import { ListLayoutColumnVO } from '@/api';
import TableCache from '@/components/table-cache';
import ProjectIssueTable from './components/project-issue-table';

const disabledSystemOptionsCodes = ['summary', 'workTime', 'historyWorkTime', 'estimatedWorkTime', 'rate'];

const defaultVisibleColumns = [
  'summary',
  'issueNum',
  'status',
  'workTime',
  'historyWorkTime',
  'estimatedWorkTime',
  'rate',
];

const defaultVisibleListLayoutColumns = defaultVisibleColumns.map((code) => ({
  columnCode: code,
  display: true,
}));

const WorkingHoursIssue = (props = {}) => {
  // @ts-ignore
  const { cached } = props;
  const {
    loadData, dateSearchDs, loading, workingHoursIssuesDs, mode, isProject, isContain, setIsContain, tableFields: fields, totalWorkTime,
  } = useIssueStore();

  const handleChangeIsContain = useCallback((value) => {
    setIsContain(value);
  }, [setIsContain]);

  if (mode === 'issue' && !defaultVisibleListLayoutColumns.find((item) => item.columnCode === 'assignee')) {
    defaultVisibleListLayoutColumns.splice(2, 0, {
      columnCode: 'assignee',
      display: true,
    });
  }
  const defaultListLayoutColumns = useMemo(() => cached?.listLayoutColumns ?? defaultVisibleListLayoutColumns, [cached?.listLayoutColumns]);

  const options = useMemo(() => getColumnManageOptions(defaultListLayoutColumns, fields), [defaultListLayoutColumns, fields]);
  const visibleColumns = useMemo(() => (
    defaultListLayoutColumns.filter((f: ListLayoutColumnVO) => f.display).map((f: ListLayoutColumnVO) => f.columnCode)
  ), [defaultListLayoutColumns]);

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
            display: isProject,
            name: '列配置',
            // icon: 'view_column-o',
            handler: () => {
              openCustomColumnManageModal({
                modelProps: {
                  title: '设置列显示字段',
                },
                projectId: getProjectId(),
                value: visibleColumns,
                options: options.map((item) => ({
                  code: item.code,
                  title: item.title,
                  disabled: includes(disabledSystemOptionsCodes, item.code),
                })),
                type: 'workingHoursIssue',
              });
            },
            element: (
              <Button>
                <Icon
                  type="view_column-o"
                  style={{ fontSize: 20, marginBottom: -1 }}
                />
                <span>列配置</span>
              </Button>),
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
          <div className={styles.searchTotalRow}>
            <div className={styles.search}>
              <WorkingHoursIssueSearch loadData={() => {}} />
            </div>
            {
              totalWorkTime && (
                <div className={styles.total}>{`总计登记工时：${totalWorkTime}h`}</div>
              )
            }
          </div>
          {
            mode === 'issue' && (
            <IssueTable
             // @ts-ignore
              dataSet={workingHoursIssuesDs}
              defaultListLayoutColumns={defaultListLayoutColumns}
            />
            )
          }
          {
            mode === 'assignee' && (
              <AssigneeIssueTable defaultListLayoutColumns={defaultListLayoutColumns} />
            )
          }
          {
            mode === 'project' && (
              <ProjectIssueTable />
            )
          }
        </LoadingProvider>
      </Content>
    </Page>
  );
};

const ObserverWorkingHoursIssue = observer(WorkingHoursIssue);

const CacheWorkingHoursIssue = (props: any) => (
  <TableCache type="workingHoursIssue" projectId={getProjectId()}>
    {(cacheProps) => <ObserverWorkingHoursIssue {...props} {...cacheProps} />}
  </TableCache>
);

const Index = (props: any) => {
  const { data: myFilter, isLoading } = useDefaultMyFilter();

  return !isLoading && (
    <StoreProvider {...props} myDefaultFilter={myFilter}>
      {
        getMenuType() === 'project' ? (
          <CacheWorkingHoursIssue />
        ) : (
          <ObserverWorkingHoursIssue />
        )
      }
    </StoreProvider>
  );
};

export default Index;
