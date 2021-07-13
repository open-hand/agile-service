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
/**
 * 将以base64的图片url数据转换为Blob
 * @param {string} urlData 用url方式表示的base64图片数据
 */
export function convertBase64UrlToBlob(urlData) {
  const bytes = window.atob(urlData.split(',')[1]); // 去掉url的头，并转换为byte

  // 处理异常,将ascii码小于0的转换为大于0
  const buffer = new ArrayBuffer(bytes.length);
  const unit8Array = new Uint8Array(buffer);
  for (let i = 0; i < bytes.length; i += 1) {
    unit8Array[i] = bytes.charCodeAt(i);
  }

  return new Blob([buffer], { type: 'image/png' });
}
/**
 * 从deltaOps中获取图片数据
 * @param {DeltaOperation []} deltaOps
 */
export function getImgInDelta(deltaOps) {
  const imgBase = [];
  const formData = new FormData();
  deltaOps.forEach((item) => {
    if (item.insert && item.insert.image) {
      if (item.insert.image.split(':').length && item.insert.image.split(':')[0] === 'data') {
        imgBase.push(item.insert.image);
        formData.append('file', convertBase64UrlToBlob(item.insert.image), 'blob.png');
      }
    }
  });
  return { imgBase, formData };
}

/**
 * 将富文本中的base64图片替换为对应的url
 * @param {{url:string} []} imgUrlList 图标url对应的
 * @param {any []} imgBase base64图片数组
 * @param {*} text 富文本的文本结构
 */
export function replaceBase64ToUrl(imgUrlList, imgBase, text) {
  const deltaOps = text;
  const imgMap = {};
  imgUrlList.forEach((imgUrl, index) => {
    imgMap[imgBase[index]] = `${imgUrl}`;
  });
  deltaOps.forEach((item, index) => {
    if (item.insert && item.insert.image && imgBase.indexOf(item.insert.image) !== -1) {
      deltaOps[index].insert.image = imgMap[item.insert.image];
    }
  });
}

export async function uploadAndReplaceImg(delta) {
  if (!delta) {
    return '';
  }
  const deltaOps = delta;
  const { imgBase, formData } = getImgInDelta(deltaOps);
  if (imgBase.length) {
    const imgUrlList = await fileApi.uploadImage(formData);
    replaceBase64ToUrl(imgUrlList, imgBase, deltaOps);
  }
  const converter = new QuillDeltaToHtmlConverter(deltaOps, {});
  const html = converter.convert();
  const text = JSON.stringify(deltaOps);
  if (!validateRichText(text)) {
    Choerodon.prompt('文字过长', 'error');
    throw new Error('文字过长');
  }
  return text;
}
export async function uploadAttachment(propFileList, issueId, projectId) {
  const fileList = propFileList.filter((i) => !i.url);
  const formData = new FormData();
  fileList.forEach((file) => {
    formData.append('file', file);
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
