import React, { useEffect } from 'react';
import { Upload } from 'choerodon-ui';
import { UploadProps } from 'choerodon-ui/lib/upload/interface';
import { Button } from 'choerodon-ui/pro';
import { randomWord } from '@/utils/random';
import './UploadButton.less';

const UploadButton = (props: UploadProps) => {
  const className = randomWord(false, 32, 40);
  useEffect(() => {
    const uploadElement = document.querySelector(`.${className} .c7n-upload-select`);
    const uploadListElement = document.querySelector(`.${className} .c7n-upload-list`);
    if (uploadElement && uploadListElement) {
      uploadListElement.appendChild(uploadElement);
    }
  });
  const innerProps = {
    multiple: true,
    beforeUpload: () => false,
  };
  return (
    <Upload
      {...innerProps}
      {...props}
      className={`c7nagile-upload-button ${className}`}
    >
      <Button funcType={'raised' as any} icon="backup-o">上传附件</Button>
    </Upload>
  );
};

export default UploadButton;
