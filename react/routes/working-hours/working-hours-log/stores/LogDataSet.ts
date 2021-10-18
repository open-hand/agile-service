import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { workingHoursApiConfig } from '@/api';
import { getIsOrganization } from '@/utils/common';

const LogDataSet = (): DataSetProps => ({
  autoQuery: false,
  paging: true,
  selection: false,
  transport: {
    read: ({ params, data }) => {
      console.log(params, data);
      return getIsOrganization() ? workingHoursApiConfig.orgGetLogs(params, data) : workingHoursApiConfig.getLogs(params, data);
    },
  },
  fields: [
    { name: 'userId', type: 'string' as FieldType, label: '成员' },
    { name: 'workTime', type: 'number' as FieldType, label: '耗费时间' },
    { name: 'startDate', type: 'string' as FieldType, label: '工作日期' },
    { name: 'issueId', type: 'string' as FieldType, label: '工作项' },
    { name: 'projectId', type: 'string' as FieldType, label: '所属项目' },
    { name: 'statusId', type: 'object' as FieldType, label: '状态' },
  ],
});

export default LogDataSet;
