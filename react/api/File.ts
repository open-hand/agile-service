import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import Api from './Api';

class FileApi extends Api<FileApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  /**
    * 上传issue的附件
    * @param {*} data
    * @param {*} config
    */
  uploadFile(data: FormData, issueId: number, projectId?: number) {
    const headers = { 'content-type': 'multipart/form-data' };
    return axios({
      headers,
      method: 'post',
      url: `/agile/v1/projects/${projectId || getProjectId()}/issue_attachment`,
      data,
      params: {
        // projectId,
        issueId,
      },
    });
  }

  /**
    * 上传图片
    * @param {any} data
    */
  uploadImage(data: FormData, outside?: boolean, projectId?: string, onUploadProgress?: (progressEvent: ProgressEvent) => void) {
    const headers = { 'content-type': 'multipart/form-data' };
    return axios({
      headers,
      method: 'post',
      url: outside ? '/agile/v1/backlog_external/attachment/upload_for_address' : `${this.prefix}/issue_attachment/upload_for_address`,
      data,
      params: {
        project_id: projectId,
      },
      onUploadProgress: (progressEvent: ProgressEvent) => {
        onUploadProgress && onUploadProgress(progressEvent);
      },
    });
  }

  /**
    * 删除文件
    * @param {number} resourceId 文件资源id
    */
  deleteFile(fileId: number) {
    return this.request({
      url: `${this.prefix}/issue_attachment/${fileId}`,
      method: 'delete',

    });
  }
}

const fileApi = new FileApi();

export { fileApi };
