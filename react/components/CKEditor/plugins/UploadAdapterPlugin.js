import { fileApi } from '@/api';

class UploadAdapter {
  constructor(loader) {
    this.loader = loader;
  }

  // Starts the upload process.
  async upload() {
    const file = await this.loader.file;
    const formData = new FormData();
    formData.append('file', file);
    const urls = await fileApi.uploadImage(formData);
    return { default: urls[0] };
  }

  // Aborts the upload process.
  abort() {
    // if (this.xhr) {
    //   this.xhr.abort();
    // }
  }
}

export default function UploadAdapterPlugin(editor) {
  // eslint-disable-next-line no-param-reassign
  editor.plugins.get('FileRepository').createUploadAdapter = (loader) => new UploadAdapter(loader);
}
