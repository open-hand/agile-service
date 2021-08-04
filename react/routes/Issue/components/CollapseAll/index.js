import React from 'react';
import { Button } from 'choerodon-ui/pro';

import { observer } from 'mobx-react-lite';
import CustomIcon from '@/components/custom-icon';

function CollapseAll({
  expandAll, isExpandAll, expandAbleKeys, ...otherProps
}) {
  // 有需要展开的再显示
  if (expandAbleKeys.length === 0) {
    return null;
  }

  return (
    <Button
      {...otherProps}
      // funcType="flat"
      onClick={() => {
        expandAll(!isExpandAll);
      }}
    >
      <CustomIcon type="icon-indent" />
      <span>{isExpandAll ? '全部收起' : '全部展开' }</span>
    </Button>

  );
}
export default observer(CollapseAll);
