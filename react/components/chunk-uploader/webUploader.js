// https://github.com/fex-team/webuploader
/* eslint-disable */
import webUploader from 'webuploader';
import $, { type } from 'jquery';
import Cookies from 'universal-cookie';

const cookies = new Cookies();

const getAccessToken = () => cookies.get('access_token');

let getCurrentOrganizationId = 1;
let getCurrentProjectId = 1;

const { API_HOST } = window._env_;

class WebUploader {
  constructor() {
    this.id = 1;
    this.inited = false;
    this.params = {};
    this.combine = {};
  }

  async init({ prefixPatch, organizationId, projectId, combine }) {
    getCurrentOrganizationId = organizationId;
    getCurrentProjectId = projectId;
    this.combine = combine;
    this.checkUrl = `${API_HOST}${prefixPatch}/v1/${getCurrentOrganizationId}/upload/check-block`;
    this.uploadUrl = `${API_HOST}${prefixPatch}/v1/${getCurrentOrganizationId}/upload/save`;
    if (this.inited) return;

    $.ajaxSetup({
      beforeSend: (xhr) => {
        xhr.setRequestHeader('Authorization', this.Authorization);
        xhr.setRequestHeader('H-Tenant-Id', organizationId);
      },
    });

    webUploader.Uploader.register(
      {
        name: this.id,
        'before-send-file': 'beforeSendFile',
        'before-send': 'beforeSend',
        'after-send-file': 'afterSendFile',
      },
      {
        beforeSendFile: this.beforeSendFile.bind(this),
        beforeSend: this.beforeSend.bind(this),
        afterSendFile: this.afterSendFile.bind(this),
      }
    );

    this.initUploader();
    this.inited = true;
  }

  initUploader() {
    this.md5Arr = [];
    this.timeArr = [];
    this._uploader = webUploader.create({
      swf: `webuploader/dist/Uploader.swf`,
      server: this.uploadUrl,
      chunked: true,
      chunkSize: 1 * 1000 * 1000,
      threads: 3,
      resize: false,
      headers: {
        Authorization: this.Authorization,
      },
    });

    this._uploader.on('all', this.onAll.bind(this));
    this._uploader.on('uploadProgress', this.onUploadProgress.bind(this));
  }

  async upload(file, onProgress) {
    if (!this.inited) {
      await this.init();
    }

    if (!file) return;
    this.clearUploader();
    this._uploader.addFile(file);
    this._uploader.upload();

    this.uploadDeferred = webUploader.Deferred();
    this.onProgress = onProgress;
    return this.uploadDeferred.promise();
  }

  clearUploader() {
    if (!this._uploader) return;
    const files = this._uploader.getFiles();
    files.forEach((file) => this._uploader.removeFile(file));
  }

  emitUploadProgress(percentage, file) {
    if (!this.onProgress) return;
    this.onProgress(percentage, file);
  }

  emitUploadResult(res) {
    if (this.uploadDeferred) {
      this.uploadDeferred.resolve(res);
    }
  }

  onAll() {
    // 可获取上传状态
  }

  onUploadProgress(file, percentage) {
    // 可计算剩余时间
    // const index = this.getFileIndex(file)
    // const currentTime = new Date();
    // const timeDiff = currentTime.getTime() - this.timeArr[index].getTime();

    this.emitUploadProgress(percentage, file);
  }

  get $() {
    return window.$;
  }

  get Authorization() {
    return `Bearer ${getAccessToken()}`;
  }

  getFileIndex(file) {
    return file.id.slice(8);
  }

  beforeSendFile(file) {
    const index = this.getFileIndex(file);
    const startTime = new Date();
    this.timeArr[index] = startTime;
    const deferred = webUploader.Deferred();

    const fileMd5CheckSize = 10 * 1024 * 1024;
    function fastMd5(uploader) {
      if (!fileMd5CheckSize || fileMd5CheckSize <= 0) {
        return uploader.md5File(file);
      } else {
        return uploader.md5File(file, 0, fileMd5CheckSize);
      }
    }

    fastMd5(new webUploader.Uploader()).then((value) => {
      this.md5Arr[index] = value;
      this._uploader.options.formData.guid = value;
      this._uploader.options.formData.fileMd5 = value;
      deferred.resolve();
    });
    return deferred.promise();
  }

  beforeSend(block) {
    const index = this.getFileIndex(block.file);
    const deferred = webUploader.Deferred();
    $.ajax({
      type: 'POST',
      url: this.checkUrl,
      data: {
        chunk: block.chunk,
        chunkSize: block.end - block.start,
        guid: this.md5Arr[index],
      },
      cache: false,
      async: false,
      timeout: 1000,
      dataType: 'json',
      success(response) {
        if (response) {
          deferred.reject();
        } else {
          deferred.resolve();
        }
      },
      error() {
        deferred.reject();
      },
    });
    return deferred.promise();
  }

  setParams(params) {
    this.params = params;
  }

  async afterSendFile(file) {
    const index = this.getFileIndex(file);
    const args = this.params;

    $.ajax({
      type: 'POST',
      url: `${this.combine.url}`,
      contentType: 'application/json',
      data: JSON.stringify({
        guid: this.md5Arr[index],
        fileName: file.name,
        ...(this.combine.requestData || {})
      }),
      dataType: 'json',
      success: (data) => {
        if (data && typeof data === 'object' && data.failed) {
          this.emitUploadResult({
            success: false,
            msg: data.message,
          });
        }
        this.emitUploadResult({
          success: true,
          data,
        });
      },
      error: (res) => {
        this.emitUploadResult({
          success: false,
          msg: res.responseText,
        });
      },
    });
  }

  unRegister () {
    webUploader.Uploader.unRegister(this.id)
  }
}

export default new WebUploader();
