import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';

function IssueInfoTableDataSet(): DataSetProps {
  return {
    // autoCreate: true,
    autoQuery: false,
    paging: false,
    fields: [
      {
        name: 'appServiceId',
      },
      {
        name: 'projectId', label: '子项目',
      },
      {
        name: 'appServiceCode', label: '应用服务', type: 'string' as any, required: false,
      },
      {
        name: 'sourceTag', label: '当前tag', type: 'string' as any, dynamicProps: { required: ({ record }) => record.get('appServiceCode') },
      },
      {
        name: 'targetTag', label: '对比tag', type: 'string' as any,
      },
    ],

  };
}
export default IssueInfoTableDataSet;
