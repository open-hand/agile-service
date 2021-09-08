import React from 'react';
import { getFileSuffix } from '@/utils/common';
import './index.less';
import PdfViewer from './PdfViewer';

const officeSuffix = ['doc', 'docx', 'docm', 'dotm', 'dotx', 'ppt', 'pptx', 'ppsx', 'ppt', 'pps', 'pptm', 'potm', 'ppam', 'potx', 'ppsm', 'xls', 'xlsx', 'xlsb', 'xlsm'];
const prefixCls = 'c7n-agile-preview';
const FilePreview = ({
  url,
}) => {
  const renderPreviewContent = () => {
    if (officeSuffix.includes(getFileSuffix(url))) {
      return (
        <div className={`${prefixCls}-content-iframeWrap`}>
          <iframe title="附件预览" width="100%" height="100%" src={`https://view.officeapps.live.com/op/view.aspx?src=${encodeURIComponent(url)}`} />
        </div>
      );
    } if (getFileSuffix(url) === 'pdf') {
      return (
        <PdfViewer file={url} />
      );
    }
    return (
      <div className={`${prefixCls}-content-imageWrap`}>
        <img className={`${prefixCls}-content-image`} src={url} alt="图片附件" />
      </div>
    );
  };
  return (
    <div className={`${prefixCls}-content`}>
      {renderPreviewContent()}
    </div>
  );
};

export default FilePreview;
