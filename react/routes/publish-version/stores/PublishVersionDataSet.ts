import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { publishVersionApiConfig } from '@/api';

const PublishVersionDataSet = (): DataSetProps => ({
  autoQuery: true,
  // dataKey: 'content',
  paging: true,
  selection: undefined,
  // data: [{
  //   name: '0.4', status: '规划中', releaseDate: '2020-01-01', artifactId: 'agile-service', groupId: 'IO.Agile', AppService: 'agile-service', tag: '0.1',
  // }],
  fields: [
    { name: 'name', type: 'string' as FieldType, label: '版本名称' },
    { name: 'status', type: 'string' as FieldType, label: '状态' },
    { name: 'actualPublishDate', type: 'string' as FieldType, label: '发布日期' },
    { name: 'artifactId', type: 'string' as FieldType, label: 'artifactId' },
    { name: 'groupId', type: 'string' as FieldType, label: 'groupId' },
    { name: 'appService', type: 'string' as FieldType, label: '关联的应用服务' },
    { name: 'tag', type: 'string' as FieldType, label: '关联tag' },
  ],
  transport: {
    read: publishVersionApiConfig.loadList(),
  },
});
export default PublishVersionDataSet;
