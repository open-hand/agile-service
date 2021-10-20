import React, { useState, useContext, useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { Tooltip } from 'choerodon-ui/pro';
import { getProjectId } from '@/utils/common';
import ChunkUploader from '@/components/chunk-uploader';
import { UploadButtonNow } from '../../../CommonComponent';
import EditIssueContext from '../../stores';
import Divider from './Divider';

// eslint-disable-next-line no-underscore-dangle
const { API_HOST } = window._env_;

const IssueAttachment = observer((props) => {
  const { reloadIssue, hasPermission = true } = props;
  const { store, disabled = false } = useContext(EditIssueContext);
  const { issueId, issueAttachmentVOList = [] } = store.getIssue;
  const initialFileList = issueAttachmentVOList.map((issueAttachment) => ({
    uid: issueAttachment.attachmentId,
    name: issueAttachment.fileName,
    url: issueAttachment.url,
    userId: issueAttachment.createdBy,
  }));
  const [fileList, setFileList] = useState(initialFileList);
  useEffect(() => {
    setFileList(initialFileList);
  }, [JSON.stringify(issueAttachmentVOList)]);
  const refresh = () => {
    if (reloadIssue) {
      reloadIssue(issueId);
    }
  };

  return (
    <div id="attachment">
      <Divider />
      <div className="c7n-title-wrapper">
        <div className="c7n-title-left">
          <span>附件</span>
        </div>
        {
          (hasPermission && !disabled) ? (
            <div className="c7n-title-right">
              <Tooltip title="上传附件">
                <div>
                  <ChunkUploader
                    prefixPatch="/hfle"
                    showUploadList={false}
                    fileList={fileList}
                    setFileList={setFileList}
                    combine={{
                      url: `${API_HOST}/agile/v1/projects/${store.projectId || getProjectId()}/issue_attachment/combine`,
                      requestData: {
                        issueId,
                      },
                    }}
                  />
                </div>
              </Tooltip>
            </div>
          ) : (
            <div style={{ height: 32 }} />
          )
        }
      </div>
      <div className="c7n-content-container">
        <UploadButtonNow
          fileList={fileList}
          projectId={store.projectId}
          setFileList={setFileList}
          hasPermission={hasPermission}
          disabled={disabled}
          refresh={refresh}
          issueId={issueId}
        />
      </div>
    </div>
  );
});

export default IssueAttachment;
