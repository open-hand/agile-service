import { FieldType, DataSetSelection } from 'choerodon-ui/pro/lib/data-set/enum';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { publishVersionApiConfig } from '@/api';

const PublishVersionDataSet = (): DataSetProps => ({
  autoQuery: false,
  paging: true,
  selection: 'single' as DataSetSelection,
  fields: [
    { name: 'name', type: 'string' as FieldType, label: '版本名称' },
    { name: 'statusCode', type: 'string' as FieldType, label: '状态' },
    { name: 'actualPublishDate', type: 'string' as FieldType, label: '发布日期' },
    { name: 'artifactId', type: 'string' as FieldType, label: 'artifactId' },
    { name: 'groupId', type: 'string' as FieldType, label: 'groupId' },
    { name: 'appServiceName', type: 'string' as FieldType, label: '关联的应用服务' },
    { name: 'tagName', type: 'string' as FieldType, label: '关联tag' },
  ],
  transport: {
    read: ({ params, data }) => ({ ...publishVersionApiConfig.loadList(), params, data: { appService: true, content: data?.content } }),
  },
});
export default PublishVersionDataSet;
