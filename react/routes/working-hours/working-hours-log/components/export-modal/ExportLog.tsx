import React, {
  useCallback, useState, useEffect,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  DataSet, Form, Button, DatePicker, TreeSelect, CheckBox,
} from 'choerodon-ui/pro';
import moment from 'moment';
import { Choerodon } from '@choerodon/boot';
import { includes, map } from 'lodash';
import { toJS } from 'mobx';
import WsProgress from '@/components/ws-progress';
import styles from './ExportLog.less';
import SelectUser from '@/components/select/select-user';
import {
  getIsOrganization, getProjectId, getProjectName, getOrganizationId,
} from '@/utils/common';
import SelectProject from '@/components/select/select-project';
import { IModalProps } from '@/common/types';
import {
  IWorkingHoursData, workGroupApi, workingHoursApi, WorkingHoursExportAction,
} from '@/api';

interface IDownLoadInfo {
  id: string | null,
  fileUrl: string | null,
  creationDate: string | null,
  lastUpdateDate: string | null,
}

export interface IExportProps {
  exportDs: DataSet,
  // eslint-disable-next-line react/require-default-props
  modal?: IModalProps,
  action: WorkingHoursExportAction,
  title?: string
  orgMessageKey: string,
  proMessageKey: string,
  fileName: string
  exportFn: (data: IWorkingHoursData) => void,
  showWorkGroup?: boolean
}

const ExportLog: React.FC<IExportProps> = ({
  exportDs, action, orgMessageKey, proMessageKey, fileName, exportFn, showWorkGroup = false,
}) => {
  const [loading, setLoading] = useState(false);
  const [downloadInfo, setDownloadInfo] = useState({} as IDownLoadInfo);
  const workGroupIds = exportDs.current?.get('workGroupIds');
  const userIds = exportDs.current?.get('userIds');

  useEffect(() => {
    workingHoursApi.getLatest(action).then((res: IDownLoadInfo) => {
      if (res.id) {
        setDownloadInfo(res);
      }
    });
  }, [action]);
  const handleExportExcel = useCallback(async () => {
    let search: IWorkingHoursData = {} as IWorkingHoursData;
    if (await exportDs.current?.validate()) {
      search = exportDs.current?.toData();
    } else {
      return false;
    }
    setLoading(true);
    await exportFn({
      ...search,
      startTime: moment(search.startTime).startOf('day').format('YYYY-MM-DD HH:mm:ss'),
      endTime: moment(search.endTime).endOf('day').format('YYYY-MM-DD HH:mm:ss'),
      workGroupIds: search.userIds?.length ? undefined : search.workGroupIds,
    });
    return false;
  }, [exportDs, exportFn]);

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
        {
        showWorkGroup && getIsOrganization() && (
          <TreeSelect
            name="workGroupIds"
            placeholder="筛选工作组"
            maxTagCount={2}
            maxTagTextLength={10}
            maxTagPlaceholder={(restValues) => `+${restValues.length}...`}
            searchable
          />
        )
      }
        <SelectUser
          name="userIds"
          key={`SLE-${workGroupIds?.length}`}
          placeholder="筛选成员"
          maxTagCount={2}
          maxTagTextLength={5}
          clearButton
          level={getIsOrganization() ? 'org' : 'project'}
          request={getIsOrganization() ? ({ page, filter, requestArgs }: { page: number, filter: string, requestArgs: { selectedUserIds?: string[]}}) => workGroupApi.loadUserByGroupIds({ workGroupIds, realName: filter, userIds: requestArgs.selectedUserIds }, {
            page,
          }).then((res: any) => {
            const userList = res.content || [];
            const noFindUserIds: string[] = [];
            if (userIds.length) {
              userIds.forEach((id: string) => {
                if (!includes(map(userList, 'id'), id)) {
                  noFindUserIds.push(id);
                }
              });
            exportDs.current?.set('userIds', userIds.filter((id: string) => !includes(noFindUserIds, id)));
            }
            return ({ ...res, content: userList, list: userList });
          }) : undefined}
          help="不选择成员时，默认为选择全部成员。"
          selected={exportDs.current?.get('userIds')}
        />
        {
          showWorkGroup && getIsOrganization() && (
            <CheckBox name="exportMonthlyReport" />
          )
        }
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
        messageKey={getIsOrganization() ? orgMessageKey : proMessageKey}
        onFinish={handleFinish}
        onStart={() => setLoading(true)}
        autoDownload={{
          fileName: `${getProjectName()}${fileName}.xlsx`,
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
