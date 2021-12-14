import React, { useMemo } from 'react';
import { useTheme } from '@choerodon/master';
import { observer } from 'mobx-react-lite';
import { usePersistFn } from 'ahooks';
import IssueTable, { IssueTableProps } from '@/components/issue-table';
import { useUpdateColumnMutation } from '@/hooks/data/useTableColumns';
import { ColumnManage } from '@/components/issue-table/Component';
import { getTableColumns } from '@/components/issue-table/columns';
import AutoSize from '@/components/auto-size';
import getListLayoutColumns from './utils/getListLayoutColumns';
import getVisibleCount from './utils/getVisibleCount';

export interface IssueTableMainProps extends IssueTableProps {

}
const ROW_HEIGHT = 40;
const HEADER_HEIGHT = 45;
const FOOTER_HEIGHT = 100;
const MIN_HEIGHT = 60;
const IssueTableMain: React.FC<IssueTableMainProps> = ({
  listLayoutColumns: savedListLayoutColumns,
  fields,
  onSummaryClick,
  tableProps,
  onCreateIssue,
  onOpenCreateIssue,
  onRowClick,
  tableRef,
  typeIdChange,
  summaryChange,
  assigneeChange,
  setDefaultSprint,
  isTree,
}) => {
  const props = tableProps;
  const {
    pagination,
    ...restProps
  } = props;
  const mutation = useUpdateColumnMutation('agile');

  const listLayoutColumns = useMemo(() => getListLayoutColumns(savedListLayoutColumns, fields), [fields, savedListLayoutColumns]);

  const columns = useMemo(() => getTableColumns({
    listLayoutColumns, fields, onSummaryClick, handleColumnResize: () => { },
  }), [fields, listLayoutColumns, onSummaryClick]);

  const handleColumnResize = usePersistFn((columnWidth, dataKey) => {
    const column = columns.find((item) => item.dataIndex === dataKey);
    mutation.mutate({
      applyType: 'agile',
      listLayoutColumnRelVOS: listLayoutColumns.map((listLayoutColumn, i) => ({
        ...listLayoutColumn,
        ...listLayoutColumn.columnCode === column.code ? {
          width: columnWidth,
        } : {
          width: listLayoutColumn.width ?? 0,
        },
        sort: i,
      })),
    });
  });

  const visibleColumnCodes = useMemo(() => (listLayoutColumns.filter((c) => c.display).map((c) => c.columnCode)), [listLayoutColumns]);

  const [theme] = useTheme();

  const getHeight = usePersistFn((availableHeight:number) => {
    const visibleCount = getVisibleCount(tableProps.data, tableProps.expandedRowKeys);
    const heightFromData = visibleCount * ROW_HEIGHT + HEADER_HEIGHT;
    return Math.min(Math.max(heightFromData, MIN_HEIGHT), Math.max(10 * ROW_HEIGHT + HEADER_HEIGHT, availableHeight - FOOTER_HEIGHT));
  });

  return (
    <>
      <div style={{
        position: 'absolute',
        top: theme === 'theme4' ? '67px' : '10px',
        right: '21px',
      }}
      >
        <ColumnManage
          tooltip={false}
          value={visibleColumnCodes}
          options={columns.map(((c) => ({
            code: c.code,
            title: c.title,
            disabled: c.code === 'summary',
          })))}
        />
      </div>
      <AutoSize style={{ overflow: 'visible' }}>
        {({ height }) => (
          <IssueTable
            height={height ? getHeight(height) : undefined}
            isTree={isTree}
            listLayoutColumns={listLayoutColumns}
            tableProps={tableProps}
            fields={fields}
            tableRef={tableRef}
            onCreateIssue={onCreateIssue}
            onOpenCreateIssue={onOpenCreateIssue}
            onRowClick={onRowClick}
            typeIdChange={typeIdChange}
            summaryChange={summaryChange}
            assigneeChange={assigneeChange}
            setDefaultSprint={setDefaultSprint}
            onSummaryClick={onSummaryClick}
            onColumnResize={handleColumnResize}
          />
        )}
      </AutoSize>
    </>
  );
};
export default observer(IssueTableMain);
