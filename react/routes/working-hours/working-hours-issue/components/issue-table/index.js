import React, { useMemo } from 'react';
import { Table } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { usePersistFn } from 'ahooks';
import { useUpdateColumnMutation } from '@/hooks/data/useTableColumns';
import { ColumnManage, TableCache } from './Component';
import styles from './index.less';
import { getTableColumns, expandColumn } from '@/components/issue-table/columns';
import getListLayoutColumns from '@/routes/Issue/components/issue-table/utils/getListLayoutColumns';
import AutoSize from '@/components/auto-size';
import getColumnManageOptions from './utils/getColumnManageOptions';

const defaultVisibleColumns = [
  'summary', 'issueNum', 'statusId', 'issueTypeId', 'reporterId', 'epicId', 'progress',
  'piNameVOList', 'sprints', 'wsjf', 'teamProjects', 'creationDate', 'lastUpdateDate',
];
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

  console.log(tableColumns);

  return (
    <Table
      queryBar="none"
      autoHeight={{ type: 'maxHeight', diff: 70 }}
      columnResizable
      className={styles.featureTable}
      dataSet={dataSet}
      columns={[expandColumn, ...tableColumns]}
      onColumnResize={handleColumnResize}
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
      top: 15,
      right: 15,
    }}
    >
      <ColumnManage
        value={visibleColumns}
        options={options}
      />
    </div>
  );
}, []);

const NewComponent = observer(({
  dataSet, onSummaryClick, fields, cached,
}) => {
  const defaultListLayoutColumns = useMemo(() => cached?.listLayoutColumns ?? defaultVisibleListLayoutColumns, [cached?.listLayoutColumns]);
  const mutation = useUpdateColumnMutation('workingHoursIssue');

  const systemOptions = useMemo(() => [...dataSet.fields].filter(([code, f]) => !code.startsWith('foundation.')).map(([code, f]) => ({
    code: f.get('name'),
    title: f.get('label'),
    disabled: f.get('name') === 'summary',
  })), [dataSet.fields]);
  const totalFields = useMemo(() => [...systemOptions, ...fields], [systemOptions, fields]);

  const listLayoutColumns = useMemo(() => getListLayoutColumns(defaultListLayoutColumns, totalFields), [defaultListLayoutColumns, totalFields]);

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
        fields={totalFields}
      />
      <AutoSize style={{ overflow: 'visible' }}>
        {({ height }) => (
          <ObserverWorkingHoursIssueTable
            dataSet={dataSet}
            onSummaryClick={onSummaryClick}
            fields={totalFields}
            defaultListLayoutColumns={listLayoutColumns}
            handleColumnResize={handleColumnResize}
          />
          // <IssueTable
          //   height={height ? getHeight(height) : undefined}
          //   isTree={isTree}
          //   listLayoutColumns={listLayoutColumns}
          //   tableProps={tableProps}
          //   fields={fields}
          //   tableRef={tableRef}
          //   onCreateIssue={onCreateIssue}
          //   onOpenCreateIssue={onOpenCreateIssue}
          //   onRowClick={onRowClick}
          //   typeIdChange={typeIdChange}
          //   summaryChange={summaryChange}
          //   assigneeChange={assigneeChange}
          //   setDefaultSprint={setDefaultSprint}
          //   onSummaryClick={onSummaryClick}
          //   onColumnResize={handleColumnResize}
          // />
        )}
      </AutoSize>

    </>
  );
}, []);

const ObserverWorkingHoursIssueTable = observer(WorkingHoursIssueTable);
export default (props) => (
  <TableCache>
    {(cacheProps) => <NewComponent {...props} {...cacheProps} />}
  </TableCache>
);
