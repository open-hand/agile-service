import React, { useCallback, useMemo } from 'react';
import {
  PerformanceTable, CheckBox, Modal,
} from 'choerodon-ui/pro';
import AutoSize from '@/components/auto-size';
import {
  Observer,
} from 'mobx-react-lite';
import { getColumns } from '../../utils';
import { usePageTemplateStore } from '../../stores';

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
            indeterminate={sortTableDataSet.selected.length > 0 && (sortTableDataSet.filter((record) => record.get('allowedEditPermission')).length !== sortTableDataSet.selected.filter((record) => record.get('allowedEditPermission')).length)}
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
            disabled={!rowData.get('allowedEditPermission')}
            checked={rowData.get('allowedEditPermission') ? rowData.isSelected : false}
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
          {() => <PerformanceTable columns={visibleColumns} data={sortTableDataSet.records} height={height} style={{ border: 'none' }} />}
        </Observer>

      )}
    </AutoSize>

  );
};
export default PageTemplateTable;
