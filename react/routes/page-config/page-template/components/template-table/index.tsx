import React, { useCallback, useMemo } from 'react';
import {
  PerformanceTable, CheckBox, Modal,
} from 'choerodon-ui/pro';
import AutoSize from '@/components/auto-size';
import {
  observer,
  Observer,
} from 'mobx-react-lite';
import { getColumns } from '../../utils';
import { usePageTemplateStore } from '../../stores';
import styles from './index.less';

interface IPageTemplateTableProps {
}
const PageTemplateTable: React.FC<IPageTemplateTableProps> = () => {
  const { sortTableDataSet, pageTemplateStore } = usePageTemplateStore();
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
            disabled={!rowData.selectable}
            checked={rowData.isSelected}
            //   checked={checkValues.includes(rowData.issueId)}
            onChange={(value) => handleCheckChange(value, rowData.index)}
          />
        )}
      </Observer>
    )
    ,
  }), [handleCheckAllChange, handleCheckChange, sortTableDataSet]);

  const visibleColumns = useMemo(() => [checkBoxColumn, ...getColumns({ issueTypeId: currentIssueTypeId, loadData: handleRefresh })], [checkBoxColumn, currentIssueTypeId, handleRefresh]);
  return (
    <AutoSize>
      {({ height }) => (
        <Observer>
          {() => <PerformanceTable className={styles.table} columns={visibleColumns} data={sortTableDataSet.records} height={height} rowHeight={40} style={{ border: 'none' }} />}
        </Observer>

      )}
    </AutoSize>

  );
};
export default observer(PageTemplateTable);
