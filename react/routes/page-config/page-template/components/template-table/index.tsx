import React, { useCallback, useMemo } from 'react';
import {
  PerformanceTable, CheckBox,
} from 'choerodon-ui/pro';
import AutoSize from '@/components/auto-size';
import {
  Observer,
} from 'mobx-react-lite';
import { columns } from '../../utils';
import { usePageTemplateStore } from '../../stores';

interface IPageTemplateTableProps {
}
const PageTemplateTable: React.FC<IPageTemplateTableProps> = () => {
  const { sortTableDataSet, pageTemplateStore } = usePageTemplateStore();
  const handleCheckChange = useCallback((val: any, index: number) => {
    val ? sortTableDataSet.select(index) : sortTableDataSet.unSelect(index);
  }, [sortTableDataSet]);
  const handleCheckAllChange = useCallback((val: any) => {
    val ? sortTableDataSet.selectAll() : sortTableDataSet.unSelectAll();
  }, [sortTableDataSet]);
  const checkBoxColumn = useMemo(() => ({
    title: (
      <Observer>
        {() => (
          <CheckBox
            indeterminate={sortTableDataSet.selected.length > 0 && (sortTableDataSet.length !== sortTableDataSet.selected.length)}
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
            checked={rowData.isSelected}
                            //   checked={checkValues.includes(rowData.issueId)}
            onChange={(value) => handleCheckChange(value, rowData.index)}
          />
        )}
      </Observer>
    )
    ,
  }), [handleCheckAllChange, handleCheckChange, sortTableDataSet]);

  const visibleColumns = useMemo(() => [checkBoxColumn, ...columns], [checkBoxColumn]);
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
