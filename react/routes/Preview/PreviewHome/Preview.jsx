import React, { useContext } from 'react';
import { Icon } from 'choerodon-ui';
import { Button } from 'choerodon-ui/pro';
import queryString from 'query-string';
import { getFileSuffix } from '../../../common/utils';
import PreviewContext from './stores';
import './Preview.less';
import logo from '../../../assets/image/logo.png';

const officeSuffix = ['doc', 'docx', 'ppt', 'pptx', 'xls', 'xlsx'];

const Preview = () => {
  const { prefixCls, location } = useContext(PreviewContext);
  const searchArgs = queryString.parse(location.search);
  const { fileService, fileName, fileUrl } = searchArgs;
  const renderPreviewContent = () => {
    if (officeSuffix.includes(getFileSuffix(fileUrl))) {
      return (
        <div className={`${prefixCls}-content-iframeWrap`}>
          <iframe title="附件预览" width="100%" height="100%" src={`https://view.officeapps.live.com/op/view.aspx?src=${fileService || ''}${encodeURIComponent(fileUrl)}`} />
        </div>
      );
    } else if (getFileSuffix(fileUrl) === 'pdf') {
      return (
        <object data={`${fileService || ''}${fileUrl}`} type="application/pdf" width="100%" height="100%">
            This browser does not support PDFs. Please download the PDF to view it: 
          <a href={`${fileService || ''}${fileUrl}`}>Download PDF</a>
        </object>
      );
    } else {
      return (
        <div className={`${prefixCls}-content-imageWrap`}>
          <img className={`${prefixCls}-content-image`} src={`${fileService || ''}${fileUrl}`} alt="图片附件" />
        </div>
      );
    }
  };

  return (
    <div className={`${prefixCls}`}>
      <div className={`${prefixCls}-headerWrap`}>
        <div className={`${prefixCls}-header`}>
          <div className={`${prefixCls}-header-logoWrap`}>
            <img className={`${prefixCls}-header-logoWrap-logo`} src={logo} alt="Choerodon-logo" />
          </div>
          <Button funcType="flat" className={`${prefixCls}-header-downloadWrap`}>
            <span className={`${prefixCls}-header-downloadWrap-span`}>
              <a style={{ marginRight: 6 }} href={`${fileService || ''}${fileUrl}`}>
                <Icon type="get_app" style={{ color: '#000' }} />
                <span className={`${prefixCls}-header-downloadWrap-fileName`}>{decodeURIComponent(fileName)}</span>
              </a>
            </span>
          </Button>
        </div>
      </div>
      <div className={`${prefixCls}-content`}>
        {renderPreviewContent()}
      </div>
    </div>
  );
};

export default Preview;
