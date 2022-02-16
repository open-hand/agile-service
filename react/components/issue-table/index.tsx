import React, {
  useState, useMemo, useCallback, useRef,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  PerformanceTable, Pagination,
} from 'choerodon-ui/pro';
import type { TableProps } from 'choerodon-ui/pro/lib/table/Table';
import type { TableProps as PTableProps } from 'choerodon-ui/pro/lib/performance-table/Table';

import { usePersistFn } from 'ahooks';
import QuickCreateIssue from '@/components/QuickCreateIssue';
import { IFoundationHeader, IIssueColumnName } from '@/common/types';
import './index.less';
import { ListLayoutColumnVO } from '@/api';
import useTable from '@/hooks/useTable';
import { checkBoxColumn, expandColumn, getTableColumns as defaultGetTableColumns } from './columns';
import { Loading } from '@/components';

type PerformanceTableProps = PTableProps & Pick<TableProps, 'dataSet'>
export interface IssueTableProps extends Partial<PerformanceTableProps> {
  tableRef?: React.RefObject<any>
  onCreateIssue?: () => void
  onOpenCreateIssue?: () => void
  getTableColumns?: (...args: Parameters<typeof defaultGetTableColumns>) => ReturnType<typeof defaultGetTableColumns>,
  // dataSet: DataSet
  fields: IFoundationHeader[]
  onRowClick?: (record: any) => void
  selectedIssue?: string
  createIssue?: boolean
  visibleColumns?: IIssueColumnName[]
  listLayoutColumns: ListLayoutColumnVO[] | null
  onSummaryClick?: (data: any) => void
  typeIdChange?: (id: string) => void
  summaryChange?: (summary: string) => void
  setDefaultSprint?: (sprintId: string | undefined) => void,
  assigneeChange?: (assigneeId: string | undefined) => void
  tableProps: ReturnType<typeof useTable>
  onColumnResize?: (columnWidth: number, dataKey: string) => void
  isTree?: boolean
  /** 是否显示checkbox 列  @default true */
  isShowColumnCheck?: boolean
  paginationWrapperClassName?: string
  height?: number
}
// const mapper = (key: IIssueColumnName): string => ({
//   summary: 'issueId',
//   issueNum: 'issueNum',
//   priority: 'priorityId',
//   sprint: 'issueSprintVOS',
//   reporter: 'reporterId',
//   creationDate: 'creationDate',
//   assign: 'assigneeId',
//   status: 'statusId',
//   lastUpdateDate: 'lastUpdateDate',
//   estimatedStartTime: 'estimatedStartTime',
//   estimatedEndTime: 'estimatedEndTime',
//   label: 'label',
//   component: 'component',
//   storyPoints: 'storyPoints',
//   fixVersion: 'fixVersion',
//   influenceVersion: 'influenceVersion',
//   epic: 'epic',
//   feature: 'feature',
// }[key] || key);

const IssueTable: React.FC<IssueTableProps> = ({
  tableRef,
  onCreateIssue,
  onOpenCreateIssue,
  getTableColumns: propsGetTableColumns,
  dataSet,
  fields,
  listLayoutColumns,
  onSummaryClick,
  selectedIssue,
  createIssue = true,
  typeIdChange = () => { },
  summaryChange = () => { },
  assigneeChange = () => { },
  setDefaultSprint = () => { },
  tableProps,
  onColumnResize,
  isTree = true,
  height,
  isShowColumnCheck,
  paginationWrapperClassName,
  ...otherProps
}) => {
  const handleOpenCreateIssue = useCallback(() => {
    onOpenCreateIssue && onOpenCreateIssue();
  }, [onOpenCreateIssue]);
  const props = tableProps;
  const {
    pagination,
    ...restProps
  } = props;
  const getTableColumns = usePersistFn(propsGetTableColumns || defaultGetTableColumns);
  const columns = useMemo(() => getTableColumns({
    listLayoutColumns, fields, onSummaryClick, handleColumnResize: onColumnResize,
  }), [fields, getTableColumns, listLayoutColumns, onColumnResize, onSummaryClick]);
  const visibleColumns = useMemo(() => columns.filter((column) => column.display), [columns]);
  const checkValuesRef = useRef<string[]>();
  checkValuesRef.current = props.checkValues;
  const checkboxColumn = useMemo(() => checkBoxColumn({
    data: props.flattenData,
    checkValues: props.checkValues,
    // 引用最新的数据 确保表格 checked 可控值拿到的是最新数据
    checkValuesRef,
    handleCheckChange: props.handleCheckChange,
    handleCheckAllChange: props.handleCheckAllChange,
  }), [props.checkValues, props.flattenData, props.handleCheckAllChange, props.handleCheckChange]);
  return (
    <div className="c7nagile-issue-table">
      <PerformanceTable
        {...otherProps}
        {...restProps}
        renderLoading={(spinElement: JSX.Element) => <Loading loadId="issue-table" loading={restProps.loading} />}
        virtualized
        bordered={false}
        columns={[...isShowColumnCheck ? [checkboxColumn] : [], ...isTree ? [expandColumn] : [], ...visibleColumns]}
        height={height ?? 400}
        rowHeight={40}
        shouldUpdateScroll={false}
      />
      {createIssue && (
        <div style={{ paddingTop: 5 }}>
          <QuickCreateIssue
            onCreate={onCreateIssue}
            cantCreateEvent={handleOpenCreateIssue}
            typeIdChange={typeIdChange}
            summaryChange={summaryChange}
            assigneeChange={assigneeChange}
            setDefaultSprint={setDefaultSprint}
          />
        </div>
      )}
      <div className={paginationWrapperClassName} style={{ display: 'flex', justifyContent: 'flex-end' }}>
        <Pagination
          total={pagination.total}
          page={pagination.current}
          pageSize={pagination.pageSize}
          onChange={pagination.onChange}
          showSizeChangerLabel={false}
          showTotal={(total, range) => <span className="word">{`显示${range[0]}-${range[1]} 共 ${total}条`}</span>}
          showPager
          showQuickJumper
        />
      </div>

    </div>
  );
};
const ObserverIssueTale = observer(IssueTable);
ObserverIssueTale.defaultProps = {
  isShowColumnCheck: true,
};
export default ObserverIssueTale;
