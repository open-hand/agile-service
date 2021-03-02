import React from 'react';
import CKEditor from '@/components/CKEditor';
import './index.less';

const prefixCls = 'c7n-ckeditor-viewer';
interface EditorProps {
  value?: string
}
const Editor: React.FC<EditorProps> = ({
  value: propsValue,
}) => (
  <div className={prefixCls}>
    <CKEditor
      disabled
      toolbar={false}
      value={propsValue}
    />
  </div>
);

export default Editor;
