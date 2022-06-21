import React, { useEffect, useRef, useState } from 'react';
import { Upload, Icon } from 'choerodon-ui';
import { inject } from 'mobx-react';

import { observer } from 'mobx-react-lite';
import { Button, message } from 'choerodon-ui/pro';
import Cookies from 'universal-cookie';

import validateFile from '@/utils/File';
import webUploader from './webUploader';

import './index.less';

const cookies = new Cookies();

const getAccessToken = () => cookies.get('access_token');

// api hfle
// eslint-disable-next-line no-underscore-dangle
const { API_HOST } = window._env_;
const HZERO_HFLE = 'hfle';

const FragmentUpload = inject('AppState')(observer((props) => {
  const {
    action = `${API_HOST}/hfle/v1/files/multipart`,
    method = 'post',
    headers = {
      Authorization: `bearer ${getAccessToken()}`,
    },
    fileSize = 30 * 1024 * 1024,
    paramsData = {},
    prefixPatch = HZERO_HFLE,
    showUploadList,
    disabled = false,
    beforeUploadCallback = () => {},
    cRef,
    children,
    isMultiple,
    ...others
  } = props;
  const organizationId = props.organizationId ?? props.AppState.currentMenuType.organizationId;
  const id = props.projectId ?? props.AppState.currentMenuType.id;
  const { fileList, setFileList, combine } = others;
  const [dealUploadList, setDealUploadList] = useState();
  const webUploaderRef = useRef(null);
  const fileMap = useRef(new Map());

  // useEffect(() => {
  //   webUploaderRef.current = webUploader;
  //   webUploaderRef.current.init({
  //     prefixPatch, organizationId, projectId: id, combine,
  //   });
  // }, [combine, id, organizationId, prefixPatch]);

  useEffect(() => {
    // webUploaderRef.current.setParams(paramsData);
    webUploader.setParams(paramsData);
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
    if (!file.size) {
      message.error('文件不能为空');
      return false;
    }
    const { accept } = others;
    if (validateFile(file, accept, fileSize)) {
      setFileList((list) => [...list, {
        uid: file.uid,
        name: file.name,
      }]);
      beforeUploadCallback((list) => [...list, {
        uid: file.uid,
        name: file.name,
      }]);
    }
    return true;
  };

  const onThisChange = async (info) => {
    const { file, event } = info;
    if (event) {
      message.error(file?.error?.message ?? '上传失败');
      setFileList((list) => list.map((item) => {
        if (!item.url && item.uid === file.uid) {
          return {
            ...item,
            status: 'error',
          };
        }
        return item;
      }));
      return;
    }
    if (file.status === 'error') {
      message.error(file?.error?.message ?? '上传失败');
      setFileList((list) => list.map((item) => {
        if (!item.url && item.uid === file.uid) {
          return {
            ...item,
            status: 'error',
          };
        }
        return item;
      }));
    }

    if (file.status === 'error' || file.status === 'done') {
      webUploaderRef.current.unRegister();
    }
  };

  const webUploaderUpload = ({
    file,
  }) => webUploaderRef.current.upload(file, (percentage) => {
    setFileList((list) => list.map((item) => {
      if (!item.url && item.uid === file.uid) {
        return {
          ...item,
          percent: percentage * 100,
        };
      }
      return item;
    }));
  });

  const whileUpload = async ({
    onError, onSuccess, file,
  }) => {
    if (!file) {
      return;
    }
    const { success, data, msg } = await webUploaderUpload({
      file,
    });
    fileMap.current.delete(file.uid);
    if (success) {
      if (onSuccess) {
        onSuccess(data);
      }
      setFileList((list) => list.map((item) => {
        if (!item.url && item.uid === file.uid) {
          return {
            ...item,
            url: data.url,
            uid: data.attachmentId || data.id,
            userId: data.createdBy,
            percent: 0,
          };
        }
        return item;
      }));
    } else {
      onError(new Error(msg));
    }
    whileUpload({
      file: [...(fileMap.current.values())][0],
    });
  };

  const customRequest = async ({
    onProgress, onError, onSuccess, file,
  }) => {
    // https://github.com/react-component/upload#customrequest
    webUploaderRef.current = webUploader;
    webUploaderRef.current.init({
      prefixPatch, organizationId, projectId: id, combine,
    });

    setTimeout(() => {
      if (!webUploaderRef.current) {
        onError(new Error('上传失败'));
      } else {
        fileMap.current.set(file.uid, file);
        if (fileMap.current.size === 1) {
          whileUpload({
            onProgress, onError, onSuccess, file,
          });
        }
      }
    }, 0);
  };

  const uploadProps = {
    multiple: isMultiple === undefined ? true : isMultiple,
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
        {
          children || (
            <Button disabled={disabled} className="c7n-chunk-upload-btn">
              <Icon type="file_upload_black-o" />
            </Button>
          )
        }
      </Upload>
    </>
  );
}));

export default FragmentUpload;
