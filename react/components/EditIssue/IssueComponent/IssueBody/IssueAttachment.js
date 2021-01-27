import React, { useState, useContext, useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { handleFileUpload } from '@/utils/richText';
import { getProjectId } from '@/utils/common';
import ChunkUploader from '@/components/chunk-uploader';
import { UploadButtonNow } from '../../../CommonComponent';
import EditIssueContext from '../../stores';
import Divider from './Divider';

const IssueAttachment = observer((props) => {
  const { reloadIssue, hasPermission } = props;
  const { store, disabled } = useContext(EditIssueContext);
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

  /**
   * 上传附件
   * @param arr
   */
  const onChangeFileList = (arr) => {
    if (arr.length > 0 && arr.some((one) => !one.url)) {
      const config = {
        issueId,
        fileName: arr[0].name || 'AG_ATTACHMENT',
        projectId: getProjectId(),
      };
      handleFileUpload(arr, refresh, config);
    }
  };

  return (
    <div id="attachment">
      <Divider />
      <div className="c7n-title-wrapper">
        <div className="c7n-title-left">
          <span>附件</span>
        </div>
      </div>
      <div className="c7n-content-wrapper" style={{ marginTop: '-47px', justifyContent: 'flex-end' }}>
        <UploadButtonNow
          fileList={fileList}
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
