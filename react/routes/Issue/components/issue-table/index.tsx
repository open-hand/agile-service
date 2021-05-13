import React, { useMemo } from 'react';
import { useTheme } from '@choerodon/master';
import { observer } from 'mobx-react-lite';
import IssueTable, { IssueTableProps } from '@/components/issue-table';
import useIsInProgram from '@/hooks/useIsInProgram';
import { useUpdateColumnMutation } from '@/hooks/data/useTableColumns';
import { usePersistFn } from 'ahooks';
import { ColumnManage } from '@/components/issue-table/Component';
import { getTableColumns } from '@/components/issue-table/columns';
import getListLayoutColumns from './utils/getListLayoutColumns';

export interface IssueTableMainProps extends IssueTableProps {

}

const IssueTableMain: React.FC<IssueTableMainProps> = ({
  listLayoutColumns: savedListLayoutColumns,
  fields,
  onSummaryClick,
  tableProps,
  onCreateIssue,
  onRowClick,
  tableRef,
  typeIdChange,
  summaryChange,
  IssueStore,
  isTree,
}) => {
  const props = tableProps;
  const {
    pagination,
    ...restProps
  } = props;
  const mutation = useUpdateColumnMutation('agile');

  const listLayoutColumns = useMemo(() => getListLayoutColumns(savedListLayoutColumns, fields), [fields, savedListLayoutColumns]);

  const handleColumnResize = usePersistFn((columnWidth, dataKey) => {
    mutation.mutate({
      applyType: 'agile',
      listLayoutColumnRelVOS: listLayoutColumns.map((listLayoutColumn, i) => ({
        ...listLayoutColumn,
        ...listLayoutColumn.columnCode === dataKey ? {
          width: columnWidth,
        } : {},
      })),
    });
  });
  const { isInProgram } = useIsInProgram();

  const visibleColumnCodes = useMemo(() => (listLayoutColumns.filter((c) => c.display).map((c) => c.columnCode)), [listLayoutColumns]);
  const [theme] = useTheme();
  const columns = useMemo(() => getTableColumns({
    listLayoutColumns, fields, onSummaryClick, handleColumnResize: () => {},
  }), [fields, listLayoutColumns, onSummaryClick]);
  return (
    <>
      <div style={{
        position: 'absolute',
        top: theme === 'theme4' ? '77px' : '10px',
        right: '21px',
      }}
      >
        <ColumnManage
          value={visibleColumnCodes}
          options={columns.map(((c) => ({
            code: c.dataIndex,
            title: c.title,
          })))}
        />
      </div>
      <IssueTable
        isTree={isTree}
        listLayoutColumns={listLayoutColumns}
        tableProps={tableProps}
        fields={fields}
        tableRef={tableRef}
        onCreateIssue={onCreateIssue}
        onRowClick={onRowClick}
        typeIdChange={typeIdChange}
        summaryChange={summaryChange}
        IssueStore={IssueStore}
        onSummaryClick={onSummaryClick}
        onColumnResize={handleColumnResize}
      />
    </>
  );
};
export default observer(IssueTableMain);
