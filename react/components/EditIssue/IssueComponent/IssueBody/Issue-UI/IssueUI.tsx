import React, {
  useEffect, useContext, useCallback, useState,
} from 'react';
import { observer } from 'mobx-react-lite';
import { WSHandler, Choerodon } from '@choerodon/boot';
import { Divider, Tooltip } from 'choerodon-ui';
import { Button } from 'choerodon-ui/pro';
import { getProjectId } from '@/utils/common';
import LinkedItem from './components/linked-item';
import UploadUI from './components/upload';
import openLinkUI from './components/link';
import './IssueUI.less';

import EditIssueContext from '../../../stores';
import { IUi } from './components/delete/DeleteUI';
import { WSItem } from './components/linked-item/LinkedItem';

const IssueUI = (props: any) => {
  const [wsData, setWsData] = useState<WSItem[]>([]);
  const { store, disabled } = useContext(EditIssueContext);
  const { issueId } = store.getIssue;

  useEffect(() => {
    store.getLinkedUI();
  }, [store]);

  const handleLinkUI = useCallback(() => {
    openLinkUI({ store, reloadIssue: props.reloadIssue });
  }, [props.reloadIssue, store]);

  const handleMessage = useCallback((message) => {
    if (!message || message === 'ok') {
      return;
    }
    const data = JSON.parse(message);
    if (data) {
      setWsData(data);
      if (data.every((item: WSItem) => item.status !== 'doing')) {
        props.reloadIssue();
        store.getLinkedUI();
        if (data.every((item: WSItem) => item.status === 'success')) {
          Choerodon.prompt('上传成功');
        } else {
          Choerodon.prompt('上传失败');
        }
      }
    }
  }, [props, store]);

  const renderUploading = useCallback(() => wsData.map((item: WSItem) => (
    <LinkedItem ui={item} reloadIssue={props.reloadIssue} uploading />
  )), [props.reloadIssue, wsData]);

  const { linkedUI } = store;
  return (
    <div id="issueUI">
      <Divider />
      <div className="c7n-title-wrapper">
        <div className="c7n-title-left">
          <span>UI&UX文件</span>
        </div>
        {
          !disabled && (
          <div className="c7n-title-right">
            <Tooltip title="关联UI/UX文件">
              <Button icon="device_hub" onClick={handleLinkUI} />
            </Tooltip>
            <UploadUI {...props} uploading={wsData && !!wsData.length && wsData.some((item) => item.status === 'doing')} />
          </div>
          )
        }

      </div>
      {
        linkedUI.map((ui: IUi) => (
          <LinkedItem ui={ui} reloadIssue={props.reloadIssue} />
        ))
      }
      {
        issueId && (
          <WSHandler
            messageKey={`${`agile-static-file-${getProjectId()}-${issueId}`}`}
            onMessage={handleMessage}
          >
            {
            renderUploading()
          }
          </WSHandler>
        )
      }
    </div>
  );
};

export default observer(IssueUI);
