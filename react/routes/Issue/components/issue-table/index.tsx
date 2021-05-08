import React, {
  useContext, Fragment,
  useState, useMemo, useCallback,
} from 'react';
import { observer } from 'mobx-react-lite';
import IssueTable, { IssueTableProps } from '@/components/issue-table';

import {
  DataSet, PerformanceTable, Pagination,
} from 'choerodon-ui/pro';
import { difference, find } from 'lodash';
import QuickCreateIssue from '@/components/QuickCreateIssue';
import useIsInProgram from '@/hooks/useIsInProgram';
import { useUpdateColumnMutation } from '@/hooks/data/useTableColumns';
import { IField, IIssueColumnName } from '@/common/types';
import { TableProps } from 'choerodon-ui/pro/lib/table/Table';
import './index.less';
import { usePersistFn } from 'ahooks';
import { ListLayoutColumnVO } from '@/api';
import useTable from '@/hooks/useTable';
import { ColumnManage } from './Component';
import { checkBoxColumn, getTableColumns } from './columns';
import transverseTreeData from './utils/transverseTreeData';
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
}) => {
  const props = tableProps;
  const {
    pagination,
    //  visibleColumns,
    setVisibleColumns, ...restProps
  } = props;
  const mutation = useUpdateColumnMutation('issues.table');

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
  const columns = useMemo(() => getTableColumns({
    listLayoutColumns, fields, onSummaryClick, handleColumnResize,
  }), [fields, handleColumnResize, listLayoutColumns, onSummaryClick]);
  const visibleColumns = useMemo(() => columns.filter((column) => column.display), [columns]);

  const treeData = useMemo(() => transverseTreeData(props.data), [props.data]);
  const checkboxColumn = useMemo(() => checkBoxColumn({
    data: props.data,
    checkValues: props.checkValues,
    handleCheckChange: props.handleCheckChange,
    handleCheckAllChange: props.handleCheckAllChange,
  }), [props.checkValues, props.data, props.handleCheckAllChange, props.handleCheckChange]);

  return (
    <div>
      <ColumnManage
        value={visibleColumnCodes}
        onChange={setVisibleColumns}
        options={listLayoutColumns.map(((c) => ({
          code: c.columnCode,
          title: c.columnCode,
        })))}
      />
      <IssueTable
        tableProps={tableProps}
        fields={fields}
        tableRef={tableRef}
        onCreateIssue={onCreateIssue}
        onRowClick={onRowClick}
        typeIdChange={typeIdChange}
        summaryChange={summaryChange}
        IssueStore={IssueStore}
        onSummaryClick={onSummaryClick}
      />
    </div>

  );
};
export default observer(IssueTableMain);
