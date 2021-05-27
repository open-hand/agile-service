import React from 'react';
import { Button, Icon } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { useProjectReportContext } from '../../context';

function CollapseAll({ ...otherProps }) {
  const { store } = useProjectReportContext();
  const { hasCollapse } = store;
  // 有需要展开的再显示
  if (store.blockList.length === 0) {
    return null;
  }

  return (
    <Button
      {...otherProps}
      funcType="flat"
      onClick={() => {
        store.collapseAll(!hasCollapse);
      }}
    >
      <span>{!hasCollapse ? '全部收起' : '全部展开' }</span>
      <Icon
        type="baseline-arrow_drop_up"
        style={{
          transform: !hasCollapse ? undefined : 'rotate(180deg)',
          transition: 'transform 0.3s',
        }}
      />
    </Button>

  );
}
export default observer(CollapseAll);
