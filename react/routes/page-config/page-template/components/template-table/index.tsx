import React, { useCallback, useMemo } from 'react';
import {
  PerformanceTable, CheckBox,
} from 'choerodon-ui/pro';
import {
  observer,
  Observer,
} from 'mobx-react-lite';
import { getColumns } from '../../utils';
import { usePageTemplateStore } from '../../stores';
import styles from './index.less';

interface IPageTemplateTableProps {
  // onScroll: (top: number) => void
  tableRef: React.RefObject<PerformanceTable>
}
const PageTemplateTable: React.FC<IPageTemplateTableProps> = ({ tableRef: propsTableRef }) => {
  const { sortTableDataSet, pageTemplateStore, disabled } = usePageTemplateStore();
  const currentIssueTypeId = pageTemplateStore.currentIssueType.id;
  const handleCheckChange = useCallback((val: any, index: number) => {
    typeof (val) === 'number' ? sortTableDataSet.select(index) : sortTableDataSet.unSelect(index);
  }, [sortTableDataSet]);
  const handleCheckAllChange = useCallback((val: any) => {
    val ? sortTableDataSet.selectAll() : sortTableDataSet.unSelectAll();
  }, [sortTableDataSet]);

  const handleRefresh = useCallback(() => {
    pageTemplateStore.loadData();
  }, [pageTemplateStore]);
  const checkBoxColumn = useMemo(() => ({
    title: (
      <Observer>
        {() => (
          <CheckBox
            disabled={disabled}
            indeterminate={sortTableDataSet.selected.length > 0 && (sortTableDataSet.filter((record) => record.selectable).length !== sortTableDataSet.selected.length)}
            checked={sortTableDataSet.selected.length > 0}
            onChange={handleCheckAllChange}
          />
        )}
      </Observer>
    ),
    dataIndex: 'issueId',
    key: 'issueId',
    width: 40,
    fixed: true,
    render: ({ rowData, dataIndex, rowIndex }: any) => (
      <Observer>
        {() => (
          <CheckBox
            key={rowIndex}
            value={rowData.index}
            disabled={disabled || !rowData.selectable}
            checked={rowData.isSelected}
            //   checked={checkValues.includes(rowData.issueId)}
            onChange={(value) => handleCheckChange(value, rowData.index)}
          />
        )}
      </Observer>
    )
    ,
  }), [disabled, handleCheckAllChange, handleCheckChange, sortTableDataSet]);
  const visibleColumns = useMemo(() => [checkBoxColumn, ...getColumns({ currentIssueType: pageTemplateStore.currentIssueType, loadData: handleRefresh, disabled })], [checkBoxColumn, currentIssueTypeId, disabled, handleRefresh]);
  return (
    <PerformanceTable
      ref={(r) => {
        Object.assign(propsTableRef, { current: r });
      }}
      className={styles.table}
      disabledScroll
      columns={visibleColumns}
      data={sortTableDataSet.records}
      autoHeight
      // height={height}
      rowHeight={40}
      style={{ border: 'none', flexShrink: 0 }}
    />
  );
};
export default observer(PageTemplateTable);
