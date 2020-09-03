import React, { useContext } from 'react';
import { Icon, Button } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import Store from '../../stores';

function CollapseAll({ tableRef }) {
  const { dataSet } = useContext(Store);
  // 有需要展开的再显示
  const needShow = dataSet.some((record) => record.children);
  if (!needShow) {
    return null;
  }
  // 需要展开：有子节点并且没有展开
  const needExpand = dataSet.some((record) => record.children && !record.isExpanded);
  // 渲染到table的第一列的头里
  return (
    <Button onClick={() => {
      if (needExpand) {
        tableRef.current.tableStore.expandAll();
      } else {
        tableRef.current.tableStore.collapseAll();
      }
    }}
    >
      {needExpand ? '全部展开' : '全部收起'}
      <Icon
        type="baseline-arrow_right"
        style={{
          transform: 'rotate(90deg)',
          transition: 'transform 0.3s',
        }}
      />
    </Button>

  );
}
export default observer(CollapseAll);
