import React, {
  useEffect, useContext, useCallback, useState,
} from 'react';
import { observer } from 'mobx-react-lite';
import { WSHandler, Choerodon } from '@choerodon/boot';
import { Tooltip, Button } from 'choerodon-ui/pro';

import { getProjectId } from '@/utils/common';
import SingleFileUpload from '@/components/SingleFileUpload';
import to from '@/utils/to';
import UploadUI from './components/upload';
import openLinkUI from './components/link';
import Divider from '../Divider';
import './IssueUI.less';

import EditIssueContext from '../../../stores';
import openDeleteModal, { IUi } from './components/delete/DeleteUI';
import { WSItem } from './components/linked-item/LinkedItem';

const IssueUI = (props: any) => {
  const [wsData, setWsData] = useState<WSItem[]>([]);
  const { store, disabled } = useContext(EditIssueContext);
  const { issueId } = store.getIssue;
  const { outside } = store;

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
      store.setLinkedUI([...(store.linkedUI.filter((item: IUi) => item.url)), ...data]);
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

  // const renderUploading = useCallback(() => wsData.map((item: WSItem) => (
  //   <LinkedItem ui={item} reloadIssue={props.reloadIssue} uploading />
  // )), [props.reloadIssue, wsData]);

  const handlePreview = useCallback((ui) => {
    if (ui.id) {
      if (!outside) {
        to(`/agile/ui-preview/${ui.id}`, {
          type: 'project',
          params: {
            fullPage: 'true',
          },
        },
        { blank: true });
      } else {
        window.open(`/#/agile/outside/ui-preview/${ui.id}`);
      }
    }
  }, [outside]);

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
            <div style={{
              display: 'flex',
              alignItems: 'center',
            }}
            >
              <Tooltip title="关联UI/UX文件">
                <Button icon="device_hub" onClick={handleLinkUI} style={{ marginRight: 16 }} />
              </Tooltip>
              <UploadUI {...props} uploading={wsData && !!wsData.length && wsData.some((item) => item.status === 'doing')} />
            </div>
          </div>
          )
        }

      </div>
      <div className="c7n-content-container">
        {
          linkedUI.map((ui: IUi | WSItem) => (
            <SingleFileUpload
              key={ui.id}
              url={ui.url}
              fileName={ui.fileName}
              onDeleteFile={() => openDeleteModal({ ui: ui as IUi, store, reloadIssue: props.reloadIssue })}
              hasDeletePermission={!disabled}
              percent={ui.status === 'doing' ? Number((ui.process * 100).toFixed(2)) : 0}
              error={ui.status === 'failed'}
              onPreview={() => handlePreview(ui)}
              isUI={ui.url}
            />
          ))
        }
      </div>

      {
        issueId && (
          <WSHandler
            messageKey={`${`agile-static-file-${store.projectId}-${issueId}`}`}
            onMessage={handleMessage}
          >
            <div />
            {/* {
              renderUploading()
            } */}
          </WSHandler>
        )
      }
    </div>
  );
};

export default observer(IssueUI);
