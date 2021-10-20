import React from 'react';
import PropTypes from 'prop-types';
import {
  Upload, Button, Icon, Tooltip,
} from 'choerodon-ui';
import { stores, Choerodon } from '@choerodon/boot';
import { fileApi } from '@/api';
import ChunkUploader from '@/components/chunk-uploader';
import SingleFileUpload from '../SingleFileUpload';
import './UploadButtonNow.less';
import { getProjectId } from '@/utils/common';

// eslint-disable-next-line no-underscore-dangle
const { API_HOST } = window._env_;

const { AppState } = stores;
const propTypes = {
  onRemove: PropTypes.func,
  beforeUpload: PropTypes.func,
};
/**
 *
 * hasPermission 进行删除权限控制，无传入，则默认为true
 */
function UploadButtonNow(props) {
  const {
    issueId, fileList, setFileList, hasPermission = true, disabled = false, refresh, projectId,
  } = props;

  const handleRemove = (file) => {
    const index = fileList.indexOf(file);
    const newFileList = fileList.slice();
    if (file.url) {
      fileApi.project(projectId).deleteFile(file.uid)
        .then((response) => {
          if (response) {
            newFileList.splice(index, 1);
            setFileList(newFileList);
            Choerodon.prompt('删除成功');
          }
        })
        .catch(() => {
          Choerodon.prompt('删除失败，请稍后重试');
        });
    } else {
      newFileList.splice(index, 1);
      setFileList(newFileList);
    }
  };

  return (
    <div className="c7n-agile-uploadButtonNow">
      <div className="c7n-agile-uploadButtonNow-fileList">
        {
          fileList && fileList.length > 0 && fileList.map((item) => (
            <SingleFileUpload
              key={item.uid}
              url={item.url}
              fileName={item.name}
              onDeleteFile={() => { handleRemove(item); }}
              hasDeletePermission={(hasPermission || AppState.userInfo.id === item.userId) && !disabled}
              percent={!item.url && (item.percent || 0)}
              error={!item.url && item.status === 'error'}
            />
          ))
        }
      </div>
    </div>
  );
}

UploadButtonNow.propTypes = propTypes;
export default UploadButtonNow;
