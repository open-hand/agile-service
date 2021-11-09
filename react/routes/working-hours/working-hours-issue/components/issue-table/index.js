import React, { useMemo } from 'react';
import { Table } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { usePersistFn } from 'ahooks';
import { includes } from 'lodash';
import { useUpdateColumnMutation } from '@/hooks/data/useTableColumns';
import { ColumnManage, TableCache } from './Component';
import styles from './index.less';
import { getTableColumns, expandColumn } from '@/components/issue-table/columns';
import getListLayoutColumns from '@/routes/Issue/components/issue-table/utils/getListLayoutColumns';
import AutoSize from '@/components/auto-size';
import getColumnManageOptions from './utils/getColumnManageOptions';

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
  dataSet, onSummaryClick, fields, defaultListLayoutColumns, handleColumnResize,
}) => {
  const visibleColumns = useMemo(() => defaultListLayoutColumns.filter((item) => item.display), [defaultListLayoutColumns]);
  const tableColumns = useMemo(() => getTableColumns({
    listLayoutColumns: visibleColumns, fields, onSummaryClick,
  }), [visibleColumns, fields, onSummaryClick]);

  console.log('tableColumns');
  console.log(tableColumns);
  return (
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
      rowHeight={30}
      selectionMode="none"
    />
  );
};

const ColumnManageComponent = observer(({
  defaultListLayoutColumns, fields,
}) => {
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
  dataSet, onSummaryClick, fields, cached,
}) => {
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
        fields={fields}
      />
      <ObserverWorkingHoursIssueTable
        dataSet={dataSet}
        onSummaryClick={onSummaryClick}
        fields={fields}
        defaultListLayoutColumns={listLayoutColumns}
        handleColumnResize={handleColumnResize}
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
