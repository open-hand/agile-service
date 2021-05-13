import { publishVersionApiConfig, versionApiConfig } from '@/api';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';

function IssueInfoTableDataSet(): DataSetProps {
  return {
    // autoCreate: true,
    autoQuery: false,
    paging: false,
    fields: [
      // {
      //   name: 'lastAppService', label: '选择应用服务', type: 'string' as any, required: true,
      // },
      {
        name: 'appServiceId',
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
    transport: {
      // read: () => ({
      //   ...publishVersionApiConfig.loadCompareHistory(store.getCurrentData.id),
      //   transformResponse: (res) => {
      //     const data = JSONbigString.parse(res);

      //     return data.map((item: any) => {
      //       const appServiceObject = store.getAppServiceList.find((service) => service.code === item.appServiceCode) || item.appServiceCode;
      //       console.log('appServiceObject', appServiceObject);
      //       return ({
      //         appServiceObject, appServiceId: appServiceObject?.id, appServiceCode: item.appServiceCode, sourceTag: item.source, targetTag: item.target,
      //       });
      //     });
      //   },
      // }),
      // submit: ({ data }) => ({ ...publishVersionApiConfig.compareTag(store.getCurrentData.id, data) }),
    },
  };
}
export default IssueInfoTableDataSet;
