import React, { useMemo, useCallback } from 'react';
import { Table } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { usePersistFn } from 'ahooks';
import { includes, get } from 'lodash';
import { useUpdateColumnMutation } from '@/hooks/data/useTableColumns';
import { ColumnManage, TableCache } from './Component';
import styles from './index.less';
import { getTableColumns, expandColumn } from '@/components/issue-table/columns';
import getListLayoutColumns from '@/routes/Issue/components/issue-table/utils/getListLayoutColumns';
import AutoSize from '@/components/auto-size';
import getColumnManageOptions from './utils/getColumnManageOptions';
import DetailContainer, { useDetail } from '@/components/detail-container';
import { StoreProvider, useIssueStore } from '../../stores';

const defaultVisibleColumns = [
  'summary',
  'issueNum',
  'status',
  'workTime',
  'historyWorkTime',
  'estimatedWorkTime',
  'rate',
];

const disabledSystemOptionsCodes = ['summary', 'workTime', 'historyWorkTime', 'estimatedWorkTime', 'rate'];
const defaultVisibleListLayoutColumns = defaultVisibleColumns.map((code) => ({
  columnCode: code,
  display: true,
}));

const WorkingHoursIssueTable = ({
  dataSet, defaultListLayoutColumns, handleColumnResize, ...otherProps
}) => {
  const [issueDetailProps] = useDetail();
  const { isProject, tableFields: fields, onCloseDetail } = useIssueStore();
  const detailCallback = useCallback(() => {
    dataSet.query(dataSet.currentPage);
    onCloseDetail();
  }, [dataSet, onCloseDetail]);
  const onSummaryClick = useCallback((record) => {
    issueDetailProps?.open({
      path: 'issue',
      props: {
        issueId: get(record, 'issueId'),
        projectId: get(record, 'projectId'),
        applyType: 'agile',
        disabled: !isProject,
      },
      events: {
        delete: detailCallback,
        close: detailCallback,
      },
    });
  }, [detailCallback, isProject, issueDetailProps]);
  const visibleColumns = useMemo(() => defaultListLayoutColumns.filter((item) => item.display), [defaultListLayoutColumns]);
  const tableColumns = useMemo(() => getTableColumns({
    listLayoutColumns: visibleColumns, fields, onSummaryClick,
  }), [visibleColumns, fields, onSummaryClick]);

  return (
    <div {...otherProps}>
      <Table
        queryBar="none"
        autoHeight={{ type: 'maxHeight', diff: 70 }}
        columnResizable
        className={styles.workingHoursIssueTable}
        dataSet={dataSet}
        columns={[...tableColumns].map((column, i) => ({
          ...column,
          renderer: column.render && (({ record }) => column.render({ rowData: record.data })),
          lock: column.fixed,
          name: column.dataIndex,
        }))}
        onColumnResize={handleColumnResize}
        mode="tree"
        rowHeight={29}
        selectionMode="none"
      />
      <DetailContainer {...issueDetailProps} />
    </div>
  );
};

const ColumnManageComponent = observer(({
  defaultListLayoutColumns,
}) => {
  const { tableFields: fields } = useIssueStore();
  const options = useMemo(() => getColumnManageOptions(defaultListLayoutColumns, fields), [defaultListLayoutColumns, fields]);
  const visibleColumns = useMemo(() => (
    defaultListLayoutColumns.filter((f) => f.display).map((f) => f.columnCode)
  ), [defaultListLayoutColumns]);
  return (
    <div style={{
      position: 'absolute',
      zIndex: 1000,
    }}
    >
      <ColumnManage
        value={visibleColumns}
        options={options.map((item) => ({
          code: item.code,
          title: item.title,
          disabled: includes(disabledSystemOptionsCodes, item.code),
        }))}
      />
    </div>
  );
}, []);

const NewComponent = observer(({
  dataSet, cached, ...otherProps
}) => {
  const { tableFields: fields } = useIssueStore();
  const defaultListLayoutColumns = useMemo(() => cached?.listLayoutColumns ?? defaultVisibleListLayoutColumns, [cached?.listLayoutColumns]);

  const mutation = useUpdateColumnMutation('workingHoursIssue');

  const listLayoutColumns = useMemo(() => getListLayoutColumns(defaultListLayoutColumns, fields), [defaultListLayoutColumns, fields]);
  const handleColumnResize = usePersistFn(({ name, width }) => {
    mutation.mutate({
      applyType: 'workingHoursIssue',
      listLayoutColumnRelVOS: listLayoutColumns.map((listLayoutColumn, i) => ({
        ...listLayoutColumn,
        ...listLayoutColumn.columnCode === name ? {
          width,
        } : {
          width: listLayoutColumn.width ?? 0,
        },
        sort: i,
      })),
    });
  });
  return (
    <>
      <ColumnManageComponent
        defaultListLayoutColumns={defaultListLayoutColumns}
      />
      <ObserverWorkingHoursIssueTable
        dataSet={dataSet}
        defaultListLayoutColumns={listLayoutColumns}
        handleColumnResize={handleColumnResize}
        {...otherProps}
      />
    </>
  );
}, []);

const ObserverWorkingHoursIssueTable = observer(WorkingHoursIssueTable);
export default (props) => (
  <TableCache>
    {(cacheProps) => <NewComponent {...props} {...cacheProps} />}
  </TableCache>
);
