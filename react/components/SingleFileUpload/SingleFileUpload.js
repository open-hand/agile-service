/* eslint-disable no-const-assign */
/* eslint-disable react/jsx-no-bind */
/* eslint-disable jsx-a11y/anchor-is-valid */
/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */
import React from 'react';
import PropTypes from 'prop-types';
import { Icon, Tooltip } from 'choerodon-ui';
import { Modal, Progress } from 'choerodon-ui/pro';
import { getFileSuffix } from '@/utils/common';
import Preview from '@/components/Preview';
import './SingleFileUpload.less';
import FileSaver from 'file-saver';
import { useDetailContainerContext } from '../detail-container/context';
import doc from './image/doc.svg';
import html from './image/html.svg';
import jpg from './image/jpg.svg';
import obj from './image/obj.svg';
import pdf from './image/pdf.svg';
import png from './image/png.svg';
import rar from './image/rar.svg';
import txt from './image/txt.svg';
import xls from './image/xls.svg';
import zip from './image/zip.svg';

const previewSuffix = ['doc', 'docx', 'ppt', 'pptx', 'xls', 'xlsx', 'pdf', 'jpg', 'jpeg', 'gif', 'png'];
const suffixImgMap = new Map([
  ['doc', doc],
  ['docx', doc],
  ['html', html],
  ['jpg', jpg],
  ['jpeg', jpg],
  ['pdf', pdf],
  ['png', png],
  ['rar', rar],
  ['txt', txt],
  ['xls', xls],
  ['xlsx', xls],
  ['zip', zip],
]);
const modalKey = Modal.key();
function SingleFileUplaod(props) {
  const {
    url, fileService, fileName, hasDeletePermission, onDeleteFile, percent, error, onPreview, isUI,
  } = props;
  // 可能在详情内，也可能不在
  const { setFilePreview } = useDetailContainerContext();
  const handleDownLoadFile = () => {
    FileSaver.saveAs(`${fileService || ''}${url}`, fileName);
  };

  const handlePreviewClick = (service, name, fileUrl) => {
    if (onPreview) {
      onPreview();
    } else {
      // 可能在详情内，也可能不在，不在详情，这个函数是undefined
      if (setFilePreview) {
        setFilePreview({
          url: `${fileService || ''}${fileUrl}`,
          name,
        });
        return;
      }
      Modal.open({
        key: modalKey,
        title: '预览',
        footer: (okBtn, cancelBtn) => null,
        className: 'c7n-agile-preview-Modal',
        cancelText: '关闭',
        fullScreen: true,
        children: <Preview
          fileName={name}
          url={`${fileService || ''}${fileUrl}`}
        />,
      });
    }
  };

  const suffix = getFileSuffix(url);

  const previewAble = ((url && previewSuffix.includes(suffix)) || isUI);
  return (
    <div className="c7n-agile-singleFileUpload-container">
      <div className="c7n-agile-singleFileUpload">
        <div style={{ display: 'flex', alignItems: 'center', maxWidth: 'calc(100% - 105px)' }}>
          <img src={suffixImgMap.get(suffix) || obj} alt="doc" className="c7n-agile-singleFileUpload-img" />
          <Tooltip title={fileName}>
            <span
              className="c7n-agile-singleFileUpload-fileName"
            >
              {fileName}
            </span>
          </Tooltip>
        </div>
        <div style={{ display: 'flex', alignItems: 'center' }}>
          {
          previewAble && (
          <span
            onClick={handlePreviewClick.bind(this, fileService, fileName, url)}
            style={{
              cursor: 'pointer',
            }}
          >
            <Tooltip title="预览">
              <Icon
                type="zoom_in"
                className="c7n-agile-singleFileUpload-icon"
                style={{ cursor: 'pointer', marginTop: -2 }}
              />
            </Tooltip>
          </span>
          )
        }
          {
          url && (
            <Tooltip title="下载">
              <Icon
                type="file_download_black-o"
                onClick={handleDownLoadFile}
              />
            </Tooltip>
          )
        }
          {(hasDeletePermission && onDeleteFile && (url || error)) && (
          <Tooltip title="删除">
            <Icon
              type="delete_sweep-o"
              onClick={() => { onDeleteFile(); }}
            />
          </Tooltip>
          )}
        </div>

      </div>
      { percent > 0 && (
        <div className={`c7n-agile-singleFileUpload-process ${error ? 'c7n-agile-singleFileUpload-errorProcess' : ''}`}>
          <Progress
            value={Number(percent.toFixed(2))}
            className="c7n-agile-singleFileUpload-process-progress"
            status={error ? 'exception' : 'active'}
          />
        </div>
      )}
    </div>
  );
}

SingleFileUplaod.defaultProps = {
  hasDeletePermission: false,
};

export default SingleFileUplaod;
