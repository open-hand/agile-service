import React, { useEffect, useContext, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import { Divider, Icon, Tooltip } from 'choerodon-ui';
import { Button } from 'choerodon-ui/pro';
import LinkedItem from './components/linked-item';
import UploadUI from './components/upload';
import openLinkUI from './components/link';
import './IssueUI.less';

import EditIssueContext from '../../../stores';
import { IUi } from './components/delete/DeleteUI';

const IssueUI = (props: any) => {
  const { store } = useContext(EditIssueContext);

  useEffect(() => {
    store.getLinkedUI();
  }, [store]);

  const handleLinkUI = useCallback(() => {
    openLinkUI({ store });
  }, [store]);

  const { linkedUI } = store;
  return (
    <div id="issueUI">
      <Divider />
      <div className="c7n-title-wrapper">
        <div className="c7n-title-left">
          <span>UI&UX文件</span>
        </div>
        <div className="c7n-title-right">
          <Tooltip title="关联UI/UX文件">
            <Button icon="device_hub" onClick={handleLinkUI} />
          </Tooltip>
          <UploadUI {...props} />
        </div>
      </div>
      {
        linkedUI.map((ui: IUi) => (
          <LinkedItem ui={ui} />
        ))
      }
    </div>
  );
};

export default observer(IssueUI);
