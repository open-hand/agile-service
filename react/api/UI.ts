import { getRequestProjectId, sameProject } from '@/utils/detail';
import { getProjectId } from '@/utils/common';
import Api from './Api';

class UiApi extends Api<UiApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  get outPrefix() {
    return '/agile/v1/backlog_external';
  }

  get isOutside() {
    return false;
  }

  outside(outside: boolean) {
    return this.overwrite('isOutside', outside);
  }

  getUIUnLinked(issueId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/static_file/${issueId} `,
    });
  }

  getLinkedUI(issueId: string) {
    return this.isOutside ? this.request({
      method: 'get',
      url: `${this.outPrefix}/related_static_file/${issueId}`,
      params: {
        project_id: this.projectId,
        organizationId: this.orgId,
      },
    }) : this.request({
      method: 'get',
      url: `/agile/v1/projects/${getRequestProjectId(this.projectId)}/${sameProject(this.projectId) ? '' : 'project_invoke_agile/related_'}static_file/${sameProject(this.projectId) ? 'related/' : ''}${issueId}`,
      params: {
        organizationId: this.orgId,
        instanceProjectId: this.projectId,
      },
    });

    // return this.request({
    //   method: 'get',
    //   url: `${this.prefix}/static_file/related/${issueId}`,
    // });
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
