/*eslint-disable */
import React, { useEffect, useRef, useState } from 'react';
import { Upload } from 'choerodon-ui';
import { inject } from 'mobx-react';
import { Icon } from 'choerodon-ui';
import { Button, message } from "choerodon-ui/pro";

import Cookies from 'universal-cookie';

import webUploader from './webUploader';

const cookies = new Cookies();

import './index.less';

const getAccessToken = () => cookies.get('access_token');

// api hfle
const { API_HOST } = window._env_;
const HZERO_HFLE = 'hfle';

const FragmentUpload = inject('AppState')((props) => {
  const {
    callback = () => {},
    action = `${API_HOST}/hfle/v1/files/multipart`,
    method = 'post',
    headers = {
      Authorization: `bearer ${getAccessToken()}`,
    },
    fileSize = 1000 * 1024 * 1024,
    paramsData = {},
    text = '上传',
    doneText = '已上传',
    type = '',
    prefixPatch = HZERO_HFLE,
    showUploadList,
    disabled = false,
    beforeUploadCallback = () => {},
    AppState: { currentMenuType: { organizationId, id } },
    cRef,
    ...others
  } = props;
 
  const { issueId, fileList, setFileList, combine } = others;
  const [dealUploadList, setDealUploadList] = useState();
  const webUploaderRef = useRef(null);
  const fileMap = useRef(new Map());

  useEffect(() => {
    webUploaderRef.current = webUploader;
    webUploaderRef.current.init({prefixPatch, organizationId, projectId: id, combine});
  }, []); // eslint-disable-line

  useEffect(() => {
    webUploaderRef.current.setParams(paramsData);
  }, [paramsData]);


  useEffect(() => {
    if (typeof showUploadList === 'boolean') {
      if (showUploadList === true) {
        const list = {
          showPreviewIcon: true,
          showRemoveIcon: false,
        };
        setDealUploadList(list);
      } else {
        setDealUploadList(showUploadList);
      }
    } else if (typeof showUploadList === 'object') {
      const list = {
        ...showUploadList,
        showRemoveIcon: false,
      };
      setDealUploadList(list);
    }
  }, [showUploadList]);

  const beforeUpload = (file) => {
    let isLimit = file.size < fileSize;
    const { accept } = others;
    if (!isLimit) {
      message.error(`只支持小于${fileSize / 1024 / 1024}MB的文件！`);
    } else if (accept && !file.name.includes(accept)) {
      message.error('上传文件类型错误');
      isLimit = false;
    } else {
      setFileList((list) => [...list, {
        uid: file.uid,
        name: file.name,
      }]);
      beforeUploadCallback((list) => [...list, {
        uid: file.uid,
        name: file.name,
      }]);
    }
    return isLimit;
  };

  const onThisChange = async (info) => {
    const { file, event } = info;
    console.log(file);
    if (event) {
      message.error(`上传失败`);
      setFileList((list) => list.map((item) => {
        if(!item.url && item.uid === file.uid) {
          return {
            ...item,
            status: 'error',
          }
        }
        return item;
      }));
      return;
    }
    if (file.status === 'error') {
      message.error(`上传失败`);
      setFileList((list) => list.map((item) => {
        if(!item.url && item.uid === file.uid) {
          return {
            ...item,
            status: 'error',
          }
        }
        return item;
      }));
      return;
    }
  };

  const webUploaderUpload = async ({
    file,
  }) => {
    return await webUploaderRef.current.upload(file, (percentage) => {
      setFileList((list) => list.map((item) => {
        if(!item.url && item.uid === file.uid) {
          return {
            ...item,
            percent: percentage * 100,
          }
        }
        return item;
      }));
    })
  }

  const whileUpload = async({
    onError, file,
  }) => {
    if(!file) {
      return;
    }
    const { success, data, msg } = await webUploaderUpload({
      file,
    });
    fileMap.current.delete(file.uid);
    if (success) {
      setFileList((list) => list.map((item) => {
        if(!item.url && item.uid === file.uid) {
          return {
            ...item,
            url: data.url,
            uid: data.attachmentId,
            percent: 0,
          }
        }
        return item;
      }));
    } else {
      console.log('offline', msg);
      onError(new Error(msg));
    }
    whileUpload({
     file: [...(fileMap.current.values())][0],
    })
  }

  const customRequest = async ({
    onProgress, onError, onSuccess, file,
  }) => {
    // https://github.com/react-component/upload#customrequest
    if (!webUploaderRef.current) {
      onError(new Error('上传失败'));
    } else {
      fileMap.current.set(file.uid, file);
      if(fileMap.current.size === 1) {
        whileUpload({
          onProgress, onError, onSuccess, file,
        });
      }
    }
  };

  const uploadProps = {
    multiple: true,
    action,
    method,
    headers,
    paramsData,
    beforeUpload,
    onChange: onThisChange,
    customRequest,
    fileList,
    ...others,
    showUploadList: dealUploadList,
  };

  return (
    <>
      <Upload 
        {...uploadProps}
        className="c7n-chunk-upload"
      >
       <Button style={{ padding: '0 6px' }}>
          <Icon type="file_upload" />
        </Button>
      </Upload>
    </>
  );
});

export default FragmentUpload;
