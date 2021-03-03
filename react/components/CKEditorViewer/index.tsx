import React, { useCallback, useState } from 'react';
import Lightbox from 'react-image-lightbox';
import CKEditor from '@/components/CKEditor';
import './index.less';

const prefixCls = 'c7n-ckeditor-viewer';

interface EditorProps {
  value?: string
  style?: React.CSSProperties
}
const Editor: React.FC<EditorProps> = ({
  value: propsValue, style,
}) => {
  const [src, setSrc] = useState<string>();
  const handleClose = useCallback(() => {
    setSrc(undefined);
  }, []);
  const handleClick = useCallback((e: React.MouseEvent<HTMLElement>) => {
    const element = e.target;
    if (element && element instanceof HTMLImageElement) {
      e.stopPropagation();
      setSrc(element.src);
    }
  }, []);

  return (
    <div role="none" className={prefixCls} onClick={handleClick} style={style}>
      <CKEditor
        disabled
        toolbar={false}
        value={propsValue}
        style={{ height: 'auto' }}
      />
      {
        src && (
          <Lightbox
            mainSrc={src}
            onCloseRequest={handleClose}
            imageTitle="图片"
          />
        )
      }
    </div>
  );
};

export default Editor;
