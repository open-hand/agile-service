import React from 'react';
import { CKEditor } from '@choerodon/components';
import { EditorProps as CKEditorProps } from '@choerodon/components/lib/ck-editor';
import { fileApi } from '@/api';

async function handleImageUpload(file: File, outside?: boolean, projectId?: string, onUploadProgress?: (progressEvent: ProgressEvent) => void) {
  const formData = new FormData();
  formData.append('file', file);
  const urls = await fileApi.project(projectId).uploadImage(formData, outside, projectId, onUploadProgress);
  return urls[0];
}
type EditorProps = Omit<CKEditorProps, 'onImageUpload'> & {
  outside?: boolean
  projectId?: string
}
const Editor: React.FC<EditorProps> = (props) => (
  <CKEditor
    {...props}
    onImageUpload={(file, onUploadProgress) => handleImageUpload(file, props.outside, props.projectId, onUploadProgress)}
  />
);

export default Editor;
