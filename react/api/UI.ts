import Api from './Api';

class UiApi extends Api<UiApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  getUIUnLinked() {
    return this.request({
      method: 'get',
      url: `${this.prefix}/static_file `,
    });
  }

  getLinkedUI(issueId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/static_file/${issueId} `,
    });
  }

  uploadUI(issueId: string, file: FormData) {
    const headers = { 'content-type': 'multipart/form-data' };
    return this.request({
      headers,
      method: 'post',
      url: `${this.prefix}/static_file`,
      params: {
        issueId,
      },
      data: file,
    });
  }

  linkUI(data: { issueId: string, fileHeaderIds: string[] }) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/static_file/related`,
      data,
    });
  }

  deleteLink(fileHeaderId: string) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/static_file/related/${fileHeaderId}`,
    });
  }

  deleteUI(fileHeaderId: string) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/static_file/${fileHeaderId}`,
    });
  }
}

const uiApi = new UiApi();

export { uiApi };
