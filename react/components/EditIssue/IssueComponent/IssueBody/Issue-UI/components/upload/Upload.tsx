import React, { useContext, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import { Tooltip, Icon, Upload } from 'choerodon-ui';
import { Button } from 'choerodon-ui/pro';
import { Choerodon } from '@choerodon/boot';
import { uiApi } from '@/api';
import styles from './Upload.less';
import EditIssueContext from '../../../../../stores';

interface Props {
  reloadIssue: Function,
  hasPermission?: boolean,
}

const UploadUI: React.FC<Props> = (props) => {
  const { reloadIssue, hasPermission = true } = props;
  const { store, disabled } = useContext(EditIssueContext);
  const { issueId } = store.getIssue;
  const { linkedUI } = store;

  const config = {
    multiple: true,
    beforeUpload: (file: File) => {
      if (file.size > 1024 * 1024 * 30) {
        Choerodon.prompt('文件不能超过30M');
        return false;
      } if (file.name && encodeURI(file.name).length > 210) {
        Choerodon.prompt('文件名过长，建议不超过20个字');
        return false;
      }
      const tmp = file;
      // @ts-ignore
      tmp.status = 'done';
      if (linkedUI.length > 0) {
        handleUpdate(linkedUI.slice().concat(file));
      } else {
        handleUpdate([file]);
      }

      return false;
    },

  };

  const handleUIUpload = useCallback((arr) => {
    const fileList = arr.filter((item: any) => !item.url);
    const formData = new FormData();
    fileList.forEach((file: any) => {
      formData.append('file', file);
    });
    uiApi.uploadUI(issueId, formData)
      .then(() => {
        Choerodon.prompt('上传成功');
        store.getLinkedUI();
        reloadIssue();
      })
      .catch(() => {
        const temp = arr.slice();
        temp.forEach((one: any) => {
          if (!one.url) {
            const tmp = one;
            tmp.status = 'error';
          }
        });
      });
  }, [issueId, reloadIssue, store]);

  const handleUpdate = useCallback((arr) => {
    if (arr.length > 0 && arr.some((one: any) => !one.url)) {
      handleUIUpload(arr);
    }
  }, [handleUIUpload]);

  return (
    <div className={styles.uploadUi}>
      {
        (hasPermission && !disabled) ? (
          // @ts-ignore
          <Upload
            {...config}
          >
            <Tooltip title="上传UI/UX文件" placement="topRight" autoAdjustOverflow={false} getPopupContainer={(triggerNode) => triggerNode.parentNode as HTMLElement}>
              <Button style={{ padding: '0 6px' }}>
                <Icon type="file_upload" />
              </Button>
            </Tooltip>
          </Upload>
        ) : (
          <div style={{ height: 32 }} />
        )
      }
    </div>
  );
};

export default observer(UploadUI);
