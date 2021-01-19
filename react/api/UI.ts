import Api from './Api';

class UiApi extends Api<UiApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  getUIUnLinked(issueId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/static_file/${issueId} `,
    });
  }

  getLinkedUI(issueId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/static_file/related/${issueId}`,
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

  deleteUI(fileHeaderId: string) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/static_file/${fileHeaderId}`,
    });
  }

  deleteLink(issueId: string, fileHeaderId: string) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/static_file/related/${issueId}/${fileHeaderId}`,
    });
  }
}

const uiApi = new UiApi();

export { uiApi };
