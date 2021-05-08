// @ts-nocheck
import React, { useState, useMemo, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
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
import ColumnManage from '@/components/column-manage';
import { usePersistFn } from 'ahooks';
import getColumnsMap, { normalColumn, checkBoxColumn } from './columns';
import transverseTreeData from './utils/transverseTreeData';

interface Props extends Partial<TableProps> {
  tableRef?: React.RefObject<any>
  onCreateIssue?: () => void
  dataSet: DataSet
  fields: IField[]
  onRowClick?: (record: any) => void
  selectedIssue?: string
  createIssue?: boolean
  visibleColumns?: IIssueColumnName[]
}
const mapper = (key: IIssueColumnName): string => ({
  summary: 'issueId',
  issueNum: 'issueNum',
  priority: 'priorityId',
  sprint: 'issueSprintVOS',
  reporter: 'reporterId',
  creationDate: 'creationDate',
  assign: 'assigneeId',
  status: 'statusId',
  lastUpdateDate: 'lastUpdateDate',
  estimatedStartTime: 'estimatedStartTime',
  estimatedEndTime: 'estimatedEndTime',
  label: 'label',
  component: 'component',
  storyPoints: 'storyPoints',
  fixVersion: 'fixVersion',
  influenceVersion: 'influenceVersion',
  epic: 'epic',
  feature: 'feature',
}[key] || key);

const IssueTable: React.FC<Props> = ({
  tableRef,
  onCreateIssue,
  dataSet,
  fields,
  listLayoutColumns,
  onSummaryClick,
  selectedIssue,
  createIssue = true,
  typeIdChange = () => { },
  summaryChange = () => { },
  IssueStore,
  tableProps,
  ...otherProps
}) => {
  const columnCodes = useMemo(() => [
    'summary',
    'issueNum',
    'priority',
    'sprint',
    'reporter',
    'creationDate',
    'assign',
    'status',
    'lastUpdateDate',
    'estimatedStartTime',
    'estimatedEndTime',
    'label',
    'component',
    'storyPoints',
    'fixVersion',
    'influenceVersion',
    'epic',
    'feature',
  ], []);
  const handleOpenCreateIssue = useCallback(() => {
    IssueStore?.createQuestion(true);
  }, [IssueStore]);
  const props = tableProps;
  const {
    pagination, visibleColumns, setVisibleColumns, ...restProps
  } = props;
  const mutation = useUpdateColumnMutation('issues.table');
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
  const getColumn = useCallback((code) => getColumnsMap({ onSummaryClick }).get(code) ?? normalColumn(find(fields, { code })), [fields, onSummaryClick]);
  // 后端保存了用后端的，没保存，使用默认的
  const visibleColumnCodes = useMemo(() => (listLayoutColumns ? listLayoutColumns.filter((c) => c.display).map((c) => c.columnCode) : visibleColumns), [listLayoutColumns, visibleColumns]);
  const columns = [checkBoxColumn({
    data: props.data,
    checkValues: props.checkValues,
    handleCheckChange: props.handleCheckChange,
    handleCheckAllChange: props.handleCheckAllChange,
  })].concat(visibleColumnCodes.map((code) => {
    const column = getColumn(code);
    const saved = find(listLayoutColumns, { columnCode: code });
    return column ? {
      ...getColumn(code), resizable: true, onResize: handleColumnResize, ...saved && saved.width > 0 ? { width: saved.width } : {},
    } : undefined;
  }).filter(Boolean));
  const totalColumnCodes = useMemo(() => [...columnCodes, ...fields.map((f) => f.code)], [columnCodes, fields]);

  const orderedCodes = useMemo(() => {
    if (listLayoutColumns) {
      const listLayoutColumnCodes = listLayoutColumns.map((c) => c.columnCode);
      // 保存在后端的在前面，但是后端保存的不一定全，所以把不在后端的补在后面
      return [...listLayoutColumnCodes, ...difference(totalColumnCodes, listLayoutColumnCodes)];
    }
    return totalColumnCodes;
  }, [listLayoutColumns, totalColumnCodes]);
  const treeData = useMemo(() => transverseTreeData(props.data), [props.data]);

  return (
    <div className="c7nagile-issue-table">
      <ColumnManage
        value={visibleColumnCodes}
        onChange={setVisibleColumns}
        options={orderedCodes.map((code) => {
          const column = getColumn(code);
          return {
            code,
            title: column?.title,
          };
        })}
      />
      <PerformanceTable
        {...restProps}
        isTree
        rowKey="issueId"
        virtualized
        bordered={false}
        columns={columns}
        // autoHeight
        height={400}
        data={treeData}
      />
      {createIssue && (
      <div style={{ paddingTop: 5 }}>
        <QuickCreateIssue
          onCreate={onCreateIssue}
          cantCreateEvent={handleOpenCreateIssue}
          typeIdChange={typeIdChange}
          summaryChange={summaryChange}
        />
      </div>
      )}
      <Pagination
        total={pagination.total}
        page={pagination.current}
        pageSize={pagination.pageSize}
        onChange={pagination.onChange}
      />
    </div>
  );
};
export default observer(IssueTable);
