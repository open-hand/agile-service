import React from 'react';
import { Button } from 'choerodon-ui/pro';

import { observer } from 'mobx-react-lite';
import CustomIcon from '@/components/custom-icon';
import useFormatMessage from '@/hooks/useFormatMessage';

function CollapseAll({
  expandAll, isExpandAll, expandAbleKeys, ...otherProps
}) {
  const formatMessage = useFormatMessage('agile.common');

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
      <span>{formatMessage({ id: isExpandAll ? 'collapse.all' : 'expand.all' })}</span>
    </Button>

  );
}
export default observer(CollapseAll);
