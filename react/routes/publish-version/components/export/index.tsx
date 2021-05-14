import React, {
  useEffect, useCallback, useState, useMemo,
} from 'react';
import {
  DataSet, Form, Modal, CheckBox,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { IModalProps } from '@/common/types';
import WsProgress from '@/components/ws-progress';
import { getProjectId, getProjectName } from '@/utils/common';
import { issueApi, publishVersionApi } from '@/api';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';

interface Props {
  modal?: IModalProps,
  publishVersionId: string
}
interface IDownLoadInfo {
  id: string | null,
  fileUrl: string | null,
  creationDate: string | null,
  lastUpdateDate: string | null,
}
const ExportPublishVersion: React.FC<Props> = observer(({ modal, publishVersionId }) => {
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'publishVersionId',
        label: '选择版本',
        // required: true,
        // multiple: true,
      },
      {
        name: 'withSubVersion',
        label: '是否包含子版本',
        // required: true,
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
      const { withSubVersion } = dataSet.current?.toData();
      await publishVersionApi.export(publishVersionId, withSubVersion);
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
    <div style={{ width: '100%', overflowX: 'hidden' }}>
      <Form dataSet={dataSet}>
        {/* <SelectPublishVersion name="publishVersionId" /> */}
        <CheckBox name="withSubVersion" />
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

const openExportPublishVersionModal = (publishVersionId: string) => {
  Modal.open({
    key: 'ExportPublishVersionModal',
    title: '导出版本',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <ExportPublishVersion publishVersionId={publishVersionId} />,
    okText: '导出',
    cancelText: '关闭',
    // okCancel: false,
  });
};
export default openExportPublishVersionModal;
