import React, {
  useEffect, useCallback, useState, useMemo,
} from 'react';
import {
  Button, DataSet, Form, Modal,
} from 'choerodon-ui/pro';
import { findIndex } from 'lodash';
import { observer } from 'mobx-react-lite';
import { IModalProps } from '@/common/types';
import WsProgress from '@/components/ws-progress';
import { getProjectId, getProjectName } from '@/utils/common';
import { issueApi, publishVersionApi } from '@/api';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import SelectPublishVersion from '@/components/select/select-publish-version';

  interface Props {
    modal?: IModalProps,
  }
  interface IDownLoadInfo {
    id: string | null,
    fileUrl: string | null,
    creationDate: string | null,
    lastUpdateDate: string | null,
  }
const ExportPublishVersion: React.FC<Props> = observer(({ modal }) => {
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'publishVersionId',
        label: '选择版本',
        required: true,
        // multiple: true,
      },

    ],
  }), []);

  const [downloadInfo, setDownloadInfo] = useState({} as IDownLoadInfo);
  const handleFinish = useCallback((messageData: any) => {
    setDownloadInfo(messageData);
      modal?.update({ okProps: { loading: false } });
  }, [modal]);

  const handleExport = useCallback(async () => {
      modal?.update({ okProps: { loading: true } });
      if (await dataSet.current?.validate()) {
        const { publishVersionId } = dataSet.current?.toData();
        await publishVersionApi.export(publishVersionId);
      }
      return false;
  }, [modal]);
  useEffect(() => {
    issueApi.loadLastImportOrExport('download_file_publish_version').then((res: any) => {
      setDownloadInfo(res);
    });
  }, []);
  useEffect(() => {
      modal?.handleOk(handleExport);
  }, [handleExport, modal]);
  return (
    <div>
      <Form dataSet={dataSet}>
        <SelectPublishVersion name="publishVersionId" />
      </Form>

      <WsProgress
        messageKey={`agile-export-publish-version-${getProjectId()}`}
        onFinish={handleFinish}
        onStart={() => { modal?.update({ okProps: { loading: true } }); }}
        autoDownload={{ fileName: `${getProjectName()}-发布版本.xlsx` }}
          // visible
        downloadInfo={downloadInfo.id ? {
          url: downloadInfo.fileUrl!,
          lastUpdateDate: downloadInfo.lastUpdateDate!,
          createDate: downloadInfo.creationDate!,
        } : undefined}
      />
    </div>
  );
});

const openExportPublishVersionModal = () => {
  Modal.open({
    key: 'ExportPublishVersionModal',
    title: '导出版本',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <ExportPublishVersion />,
    okText: '导出',
    cancelText: '关闭',
    // okCancel: false,
  });
};
export default openExportPublishVersionModal;
