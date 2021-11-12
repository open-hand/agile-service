import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { workingHoursApiConfig } from '@/api';

interface props {
}

const ProjectDataSet = () : DataSetProps => ({
  autoQuery: false,
  paging: true,
  cacheSelection: false,
  transport: {
    // @ts-ignore
    read: ({ params, data }) => workingHoursApiConfig.getLogs(params, { startTime: '2021-10-10 00:00:00', endTime: '2021-11-10 23:59:59' }),
  },
  fields: [
    { name: 'projectId', type: 'string' as FieldType, label: '名称' },
    { name: 'workTime', type: 'string' as FieldType, label: '工时' },
    { name: 'historyWorkTime', type: 'string' as FieldType, label: '历史累计工时' },
    { name: 'estimatedWorkTime', type: 'string' as FieldType, label: '预估总工时' },
    { name: 'rate', type: 'string' as FieldType, label: '偏差率' },
  ],
});

export default ProjectDataSet;
