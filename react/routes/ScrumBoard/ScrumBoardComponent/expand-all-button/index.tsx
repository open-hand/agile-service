import React, { useState, useEffect } from 'react';
import {
  Select, Spin, Icon, Modal, Form, Tooltip, Radio,
} from 'choerodon-ui';
import { C7NFormat } from '@choerodon/master';
import { Button } from 'choerodon-ui/pro';
import classnames from 'classnames';
import { observer } from 'mobx-react-lite';
import scrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { getProjectId } from '@/utils/common';
import CustomIcon from '@/components/custom-icon';
import expandStyles from './index.less';
import useFormatMessage from '@/hooks/useFormatMessage';

function ExpandAllButton(props: any) {
  const [expandAll, setExpandAll] = useState<boolean>(() => {
    const isFirstInto = !localPageCacheStore.has(new RegExp('scrumBoard.panel'));
    return !!isFirstInto;
  });

  const formatMessage = useFormatMessage('agile.common');
  function handleClick() {
    if (scrumBoardStore.currentBindFunctionMaps.has('expandOrUp-epic')) {
      scrumBoardStore.bindFunction('expand-current-status', () => !expandAll);
      scrumBoardStore.executeBindFunction(['expandOrUp-epic'], !expandAll);
      setExpandAll(!expandAll);

      return;
    }
    scrumBoardStore.removeBindFunction('expand-current-status');

    scrumBoardStore.executeBindFunction(['expandOrUp', 'expandOrUp-epic'], !expandAll);
    setExpandAll(!expandAll);
    // scrumBoardStore.currentBindFunctionMaps.get('expandOrUp')(!expandAll);
  }
  useEffect(() => () => scrumBoardStore.removeBindFunction('expand-current-status'));
  return scrumBoardStore.currentBindFunctionMaps.get('expandOrUp') || scrumBoardStore.currentBindFunctionMaps.get('expandOrUp-epic') ? (
    <Button {...props} icon="format_indent_increase" onClick={handleClick}>
      {expandAll ? (
        <span>
          {formatMessage({ id: 'collapse.all' })}

        </span>
      ) : (
        <Tooltip title="仅展开前15项">
          <span>
            {formatMessage({ id: 'expand.all' })}
          </span>
        </Tooltip>
      )}
    </Button>
  ) : null;
}
export default observer(ExpandAllButton);
