import React from 'react';
import { Icon, Button } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';

function CollapseAll({ expandAll, isExpandAll, expandAbleKeys }) {
  // 有需要展开的再显示
  if (expandAbleKeys.length === 0) {
    return null;
  }

  return (
    <Button onClick={() => {
      expandAll(!isExpandAll);
    }}
    >
      {isExpandAll ? '全部收起' : '全部展开' }
      <Icon
        type="baseline-arrow_drop_up"
        style={{
          transform: isExpandAll ? undefined : 'rotate(180deg)',
          transition: 'transform 0.3s',
        }}
      />
    </Button>

  );
}
export default observer(CollapseAll);
