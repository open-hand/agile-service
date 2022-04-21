/* eslint-disable no-unused-vars */
import { stores, Choerodon } from '@choerodon/boot';
import { DeltaOperation } from 'react-quill';
import _, { find, findIndex, chunk } from 'lodash';
import { fileApi } from '@/api';

const { AppState } = stores;
const QuillDeltaToHtmlConverter = require('quill-delta-to-html');

function validateRichText(text) {
  return !text ? true : text.length < 4000;
}

export async function uploadAttachment(propFileList, issueId, projectId) {
  const fileList = propFileList.filter((i) => !i.url);
  const formData = new FormData();
  fileList.forEach((file) => {
    formData.append('file', file?.originFileObj || file);
  });
  await fileApi.uploadFile(formData, issueId, projectId);
}
/**
 * 适用于富文本附件上传以及回调
 * @param {any []} propFileList 文件列表
 * @param {function} func 回调
 * @param {{issueType?:string,issueId:number,fileName:string}} config 附件上传的额外信息
 */
export function handleFileUpload(propFileList, func, config, projectId) {
  const fileList = propFileList.filter((i) => !i.url);
  const formData = new FormData();
  fileList.forEach((file) => {
    // file.name = encodeURI(encodeURI(file.name));
    formData.append('file', file);
  });
  fileApi.uploadFile(formData, config.issueId, projectId)
    .then((response) => {
      const newFileList = [
        {
          uid: -1,
          name: fileList[0].name,
          status: 'done',
          url: response,
        },
      ];
      Choerodon.prompt('上传成功');
      func(newFileList);
    })
    .catch((error) => {
      const temp = propFileList.slice();
      temp.forEach((one) => {
        if (!one.url) {
          const tmp = one;
          tmp.status = 'error';
        }
      });
      func(temp);
    });
}
export function text2Delta(description) {
  if (!description) {
    return undefined;
  }
  // eslint-disable-next-line no-restricted-globals
  if (!isNaN(description)) {
    return String(description);
  }
  let temp = description;
  try {
    temp = JSON.parse(description.replace(/\\n/g, '\\n')
      .replace(/\\'/g, "\\'")
      .replace(/\\"/g, '\\"')
      .replace(/\\&/g, '\\&')
      .replace(/\\r/g, '\\r')
      .replace(/\\t/g, '\\t')
      .replace(/\\b/g, '\\b')
      .replace(/\\f/g, '\\f'));
  } catch (error) {
    temp = description;
  }
  // return temp;
  return temp || '';
}

/**
 * 将quill特有的文本结构转为html
 * @param {*} delta
 */
export function delta2Html(description) {
  let isDelta = true;
  try {
    JSON.parse(description);
  } catch (error) {
    isDelta = false;
  }
  if (!isDelta) {
    return description;
  }
  const delta = text2Delta(description);
  const converter = new QuillDeltaToHtmlConverter(delta, {});
  const text = converter.convert();
  if (text.substring(0, 3) === '<p>') {
    return text.substring(3);
  }
  return text;
}

export function validateFile(rule, fileList, callback) {
  if (fileList) {
    fileList.forEach((file) => {
      if (file.size > 1024 * 1024 * 30) {
        callback('文件不能超过30M');
      } else if (file.name && encodeURI(file.name).length > 210) {
        callback('文件名过长');
      }
    });
    callback();
  } else {
    callback();
  }
}

export function normFile(e) {
  if (Array.isArray(e)) {
    return e;
  }
  return e && e.fileList;
}
