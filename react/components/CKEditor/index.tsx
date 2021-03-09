import React from 'react';
import { CKEditor } from '@choerodon/components';
import { EditorProps as CKEditorProps } from '@choerodon/components/lib/ck-editor';
import { fileApi } from '@/api';

async function handleImageUpload(file:File) {
  const formData = new FormData();
  formData.append('file', file);
  const urls = await fileApi.uploadImage(formData);
  return urls[0];
}
type EditorProps =Omit<CKEditorProps, 'onImageUpload'>
const Editor: React.FC<EditorProps> = (props) => (
  <CKEditor
    {...props}
    onImageUpload={handleImageUpload}
  />
);

export default Editor;
