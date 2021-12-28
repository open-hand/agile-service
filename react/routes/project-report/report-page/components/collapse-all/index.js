import React from 'react';
import { Button, Icon } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { useProjectReportContext } from '../../context';
import useFormatMessage from '@/hooks/useFormatMessage';

function CollapseAll({ ...otherProps }) {
  const formatMessage = useFormatMessage('agile.common');
  const { store } = useProjectReportContext();
  const { hasCollapse } = store;
  // 有需要展开的再显示
  if (store.blockList.length === 0) {
    return null;
  }

  return (
    <Button
      {...otherProps}
      // color="primary"
      // funcType="flat"
      icon="format_indent_increase"
      onClick={() => {
        store.collapseAll(!hasCollapse);
      }}
    >
      <span>{formatMessage({ id: !hasCollapse ? 'collapse.all' : 'expand.all' })}</span>
      {/* <Icon
        type="baseline-arrow_drop_up"
        style={{
          transform: !hasCollapse ? undefined : 'rotate(180deg)',
          transition: 'transform 0.3s',
        }}
      /> */}
    </Button>

  );
}
export default observer(CollapseAll);
