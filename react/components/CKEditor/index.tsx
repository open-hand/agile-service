import React from 'react';
import { CKEditor } from '@choerodon/components';
import { EditorProps as CKEditorProps } from '@choerodon/components/lib/ck-editor';
import { fileApi } from '@/api';

async function handleImageUpload(file: File, outside?: boolean, projectId?: string) {
  const formData = new FormData();
  formData.append('file', file);
  const urls = await fileApi.uploadImage(formData, outside, projectId);
  return urls[0];
}
type EditorProps = Omit<CKEditorProps, 'onImageUpload'> & {
  outside?: boolean
  projectId?: string
}
const Editor: React.FC<EditorProps> = (props) => (
  <CKEditor
    {...props}
    onImageUpload={(file) => handleImageUpload(file, props.outside, props.projectId)}
  />
);

export default Editor;
