import React from 'react';
import { Button, Icon } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
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
      icon="format_indent_increase"
      onClick={() => {
        expandAll(!isExpandAll);
      }}
    >
      <span>{formatMessage({ id: isExpandAll ? 'collapse.all' : 'expand.all' })}</span>
    </Button>

  );
}
export default observer(CollapseAll);
