import React, { useCallback } from 'react';
import { createPortal } from 'react-dom';
import { Button, Icon } from 'choerodon-ui/pro';
import { FuncType } from 'choerodon-ui/pro/lib/button/interface';
import classNames from 'classnames';
import FileSaver from 'file-saver';
import FilePreview from '@/components/Preview/FilePreview';
import { useDetailContainerContext } from '../context';
import styles from './index.less';

const PreviewIssueFile = () => {
  const {
    filePreview, setFilePreview, hidden, setHidden,
  } = useDetailContainerContext();
  const handleDownLoadFile = useCallback(() => {
    if (filePreview) {
      FileSaver.saveAs(filePreview.url, filePreview.name);
    }
  }, [filePreview]);
  if (!filePreview) {
    return null;
  }
  const element = (
    <div
      className={styles.preview_container}
    >
      <div className={styles.header}>
        <Button
          funcType={'flat' as FuncType}
          onClick={handleDownLoadFile}
        >
          {filePreview.name}
          <Icon type="get_app" style={{ marginLeft: 4 }} />
        </Button>
        <Button
          style={{
            marginLeft: 'auto',
            borderRadius: '4px',
          }}
          onClick={() => {
            setHidden(!hidden);
          }}
        >
          查看工作项详情
        </Button>
        <Button
          style={{
            borderRadius: '4px',
            marginLeft: 16,
          }}
          icon="close"
          onClick={() => {
            setFilePreview(undefined);
          }}
        />
      </div>
      <div className={classNames(styles.content, {
        [styles.detail_space]: !hidden,
      })}
      >
        <FilePreview url={filePreview.url} />
      </div>

    </div>
  );
  return createPortal(element, document.body);
};
export default PreviewIssueFile;
