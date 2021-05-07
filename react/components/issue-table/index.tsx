// @ts-nocheck
import React, { useState, useMemo, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import {
  DataSet, PerformanceTable, Pagination,
} from 'choerodon-ui/pro';
import QuickCreateIssue from '@/components/QuickCreateIssue';
import useIsInProgram from '@/hooks/useIsInProgram';
import { IField, IIssueColumnName } from '@/common/types';
import { TableProps } from 'choerodon-ui/pro/lib/table/Table';
import './index.less';
import getColumnsMap, { normalColumn, checkBoxColumn } from './columns';
import ColumnFilter from './ColumnFilter';
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
  onSummaryClick,
  selectedIssue,
  createIssue = true,
  visibleColumns = defaultVisibleColumns,
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
  const { pagination, visibleColumns, setVisibleColumns } = props;
  const { isInProgram } = useIsInProgram();
  const getColumn = useCallback((code) => getColumnsMap({ onSummaryClick }).get(code) ?? normalColumn(code), [onSummaryClick]);
  const columns = [checkBoxColumn({
    data: props.data,
    checkValues: props.checkValues,
    handleCheckChange: props.handleCheckChange,
    handleCheckAllChange: props.handleCheckAllChange,
  })].concat(visibleColumns.map((code) => getColumn(code)));
  const treeData = useMemo(() => transverseTreeData(props.data), [props.data]);
  return (
    <div className="c7nagile-issue-table">
      <ColumnFilter
        value={visibleColumns}
        onChange={setVisibleColumns}
        options={columnCodes.map((code) => {
          const column = getColumn(code);
          return {
            code,
            title: column.title,
          };
        })}
      />
      <PerformanceTable
        isTree
        rowKey="issueId"
        loading={props.loading}
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
