import React, { useState, useEffect } from 'react';
import {
  Select, Spin, Icon, Modal, Form, Tooltip, Radio,
} from 'choerodon-ui';
import { Button } from 'choerodon-ui/pro';
import classnames from 'classnames';
import scrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import { observer } from 'mobx-react-lite';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { getProjectId } from '@/utils/common';
import expandStyles from './index.less';

function ExpandAllButton(props: any) {
  const [expandAll, setExpandAll] = useState<boolean>(() => {
    const isFirstInto = !localPageCacheStore.has(new RegExp('scrumBoard.panel'));
    return !!isFirstInto;
  });

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
    <Button {...props} onClick={handleClick}>
      {expandAll ? <span>全部收起</span> : (
        <Tooltip title="仅展开前15项">
          <span>全部展开</span>
        </Tooltip>
      )}
      <Icon
        type="baseline-arrow_drop_up"
        className={classnames(expandStyles.icon, { [expandStyles.expand]: expandAll })}
      />
    </Button>
  ) : null;
}
export default observer(ExpandAllButton);
