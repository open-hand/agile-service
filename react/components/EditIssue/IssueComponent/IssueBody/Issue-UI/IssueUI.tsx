import React, { useEffect, useContext } from 'react';
import { observer } from 'mobx-react-lite';
import { Divider, Icon } from 'choerodon-ui';
import { Button } from 'choerodon-ui/pro';
import LinkedItem from './components/linked-item';
import UploadUI from './components/upload';
import './IssueUI.less';

import EditIssueContext from '../../../stores';

const IssueUI = (props: any) => {
  const { store } = useContext(EditIssueContext);

  useEffect(() => {
    store.getLinkedUI();
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
          <Button icon="device_hub" />
          <UploadUI {...props} />
        </div>
      </div>
      {
        linkedUI.map((ui: any) => (
          <LinkedItem ui={ui} />
        ))
      }
    </div>
  );
};

export default observer(IssueUI);
