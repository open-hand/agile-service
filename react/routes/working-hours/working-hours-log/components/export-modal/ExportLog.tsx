import React, {
  useCallback, useState, useEffect,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  DataSet, Form, Button, DatePicker,
} from 'choerodon-ui/pro';
import moment from 'moment';
import { Choerodon } from '@choerodon/boot';
import WsProgress from '@/components/ws-progress';
import styles from './ExportLog.less';
import SelectUser from '@/components/select/select-user';
import {
  getIsOrganization, getProjectId, getProjectName, getOrganizationId,
} from '@/utils/common';
import SelectProject from '@/components/select/select-project';
import { IModalProps } from '@/common/types';
import { ILogData, workingHoursApi } from '@/api';

interface IDownLoadInfo {
  id: string | null,
  fileUrl: string | null,
  creationDate: string | null,
  lastUpdateDate: string | null,
}

const ExportLog: React.FC<{
  exportDs: DataSet,
  // eslint-disable-next-line react/require-default-props
  modal?: IModalProps
}> = ({ exportDs, modal }) => {
  const [loading, setLoading] = useState(false);
  const [downloadInfo, setDownloadInfo] = useState({} as IDownLoadInfo);
  useEffect(() => {
    workingHoursApi.getLatest().then((res: IDownLoadInfo) => {
      if (res.id) {
        setDownloadInfo(res);
      }
    });
  }, []);
  const handleExportExcel = useCallback(async () => {
    let search: ILogData = {} as ILogData;
    if (await exportDs.current?.validate()) {
      search = exportDs.current?.toData();
    } else {
      return false;
    }
    setLoading(true);
    await workingHoursApi.exportLog({
      ...search,
      startTime: moment(search.startTime).startOf('day').format('YYYY-MM-DD HH:mm:ss'),
      endTime: moment(search.endTime).endOf('day').format('YYYY-MM-DD HH:mm:ss'),
    });
    return false;
  }, [exportDs]);

  const handleFinish = (messageData: any) => {
    Choerodon.prompt('导出成功');
    setDownloadInfo(messageData);
    setLoading(false);
  };

  return (
    <div>
      <Form dataSet={exportDs}>
        <div className={styles.dateRange}>
          <DatePicker
            name="startTime"
            style={{
              marginRight: 5,
            }}
            clearButton={false}
          />
          <span style={{ flex: 0 }}>-</span>
          <DatePicker
            name="endTime"
            style={{
              width: 120,
              marginLeft: 5,
            }}
            clearButton={false}
          />
        </div>
        {
        getIsOrganization() && (
          <SelectProject
            name="projectIds"
            multiple
            maxTagCount={2}
            maxTagTextLength={5}
            help="不选择项目时，默认为选择全部项目。"
          />
        )
      }
        <SelectUser
          name="userIds"
          maxTagCount={2}
          maxTagTextLength={5}
          clearButton
          help="不选择成员时，默认为选择全部成员。"
          selected={exportDs.current?.get('userIds')}
        />
      </Form>
      <Button
        icon="unarchive-o"
        onClick={handleExportExcel}
        loading={loading}
      >
        导出
      </Button>
      <WsProgress
        key={getProjectId() || getOrganizationId()}
        messageKey={getIsOrganization() ? `agile-export-work-hours-log-org-${getOrganizationId()}` : `agile-export-work-hours-log-${getProjectId()}`}
        onFinish={handleFinish}
        onStart={() => setLoading(true)}
        autoDownload={{
          fileName: `${getProjectName()}工时日志.xlsx`,
        }}
        downloadInfo={downloadInfo.id ? {
          url: downloadInfo.fileUrl!,
          lastUpdateDate: moment(downloadInfo.lastUpdateDate!).format('YYYY-MM-DD HH:mm:ss'),
          createDate: downloadInfo.creationDate!,
        } : undefined}
      />
    </div>
  );
};
export default observer(ExportLog);
