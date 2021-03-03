import React, { useCallback, useState } from 'react';
import Lightbox from 'react-image-lightbox';
import CKEditor from '@/components/CKEditor';
import './index.less';

const prefixCls = 'c7n-ckeditor-viewer';

const isImgElement = (element: Element): element is HTMLImageElement => element.nodeName === 'IMG';
interface EditorProps {
  value?: string
}
const Editor: React.FC<EditorProps> = ({
  value: propsValue,
}) => {
  const [src, setSrc] = useState<string>();
  const handleClose = useCallback(() => {
    setSrc(undefined);
  }, []);
  const handleClick = useCallback((e: React.MouseEvent<HTMLElement>) => {
    const element = e.target;
    if (element && element instanceof Element) {
      e.stopPropagation();
      if (isImgElement(element)) {
        setSrc(element.src);
      }
    }
  }, []);

  return (
    <div role="none" className={prefixCls} onClick={handleClick}>
      <CKEditor
        disabled
        toolbar={false}
        value={propsValue}
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
