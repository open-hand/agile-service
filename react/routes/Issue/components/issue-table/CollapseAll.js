import React, { useContext } from 'react';
import { Icon } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import Portal from '@/components/Portal';
import Store from '../../stores';

function CollapseAll({ tableRef }) {
  const { dataSet } = useContext(Store);
  // 有需要展开的再显示
  const needShow = dataSet.some(record => record.children);
  if (!needShow) {
    return null;
  }
  // 需要展开：有子节点并且没有展开
  const needExpand = dataSet.some(record => record.children && !record.isExpanded);
  // 渲染到table的第一列的头里
  return (
    <Portal target={() => document.querySelector('.c7nagile-issue-table .c7n-pro-table-thead .c7n-pro-table-cell[data-index="issueId"]')}>    
      <div 
        style={{
          position: 'absolute',
          cursor: 'pointer',
          left: 13,
          top: 0,
        }}
        onClick={() => {
          if (needExpand) {
            tableRef.current.tableStore.expandAll();
          } else {
            tableRef.current.tableStore.collapseAll();
          }
        }}
      >
        <Icon
          type="baseline-arrow_right"        
          style={{
            transform: needExpand ? 'none' : 'rotate(90deg)',
            transition: 'transform 0.3s',
          }}
        />
      </div>
    </Portal>
  );
}
export default observer(CollapseAll);
