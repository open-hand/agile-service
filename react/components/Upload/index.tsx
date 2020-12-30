import React, { ChangeEvent, useRef } from 'react';
import { Button } from 'choerodon-ui/pro';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';

interface Props {
  onUpload: (files: FileList) => void
}
const Upload: React.FC<Props> = ({ onUpload, children }) => {
  const ref = useRef<HTMLInputElement>(null);
  const handleUpload = (e: ChangeEvent<HTMLInputElement>) => {
    onUpload(e.target.files as FileList);
    if (ref.current) {
      ref.current.value = '';
    }
  };
  const handleClick = () => {
    if (ref.current) {
      ref.current.click();
    }
  };
  let trigger = <Button icon="file_upload" color={'blue' as ButtonColor} onClick={handleClick} />;
  if (children) {
    const child = React.Children.only(children);
    if (React.isValidElement(child)) {
      trigger = React.cloneElement(child, {
        onClick: handleClick,
      });
    }
  }
  return (
    <div>
      {trigger}
      <input
        ref={ref}
        type="file"
        multiple
        onChange={handleUpload}
        style={{ display: 'none' }}
      />
    </div>
  );
};

export default Upload;
