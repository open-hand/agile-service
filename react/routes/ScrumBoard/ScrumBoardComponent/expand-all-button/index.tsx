import React, { useState } from 'react';
import {
  Button, Select, Spin, Icon, Modal, Form, Tooltip, Radio,
} from 'choerodon-ui';
import classnames from 'classnames';
import scrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import { observer } from 'mobx-react-lite';
import expandStyles from './index.less';

function ExpandAllButton() {
  const [expandAll, setExpandAll] = useState<boolean>();
  function handleClick() {
    scrumBoardStore.executeBindFunction(['expandOrUp', 'expandOrUp-epic'], false);
    // scrumBoardStore.currentBindFunctionMaps.get('expandOrUp')(!expandAll);
  }
  return scrumBoardStore.currentBindFunctionMaps.get('expandOrUp') || scrumBoardStore.currentBindFunctionMaps.get('expandOrUp-epic') ? (
    <Button onClick={handleClick}>
      {`全部${expandAll || true ? '收起' : '展开'}`}
      <Icon type="baseline-arrow_right" className={classnames({ [expandStyles.expand]: expandAll })} />
    </Button>
  ) : null;
}
export default observer(ExpandAllButton);
