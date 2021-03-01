import { pageConfigApiConfig } from '@/api';
import { remove, intersectionBy } from 'lodash';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { Store } from './useStore';

interface Props {
    isEdit: boolean
    oldRecord?:Record
    store:Store
}
function ContextOptionsDataSet({ isEdit, oldRecord, store }: Props): DataSetProps {
  return {
    autoQuery: true,
    paging: false,
    transport: {
      read: ({ dataSet: optionDs }) => ({
        ...pageConfigApiConfig.loadAvailableIssueType(),
        transformResponse: (res: any) => {
          const data = JSON.parse(res).filter((item: any) => item.enabled);

          if (isEdit) {
            const disabledData = JSON.parse(res).filter((item: any) => !item.enabled);
            let issueTypeVOList = oldRecord?.get('issueTypeVOList') || [];
            issueTypeVOList = intersectionBy<any>(disabledData, issueTypeVOList, (i: any) => i.id);
            issueTypeVOList.length > 0 && store.eternalContext.push(...issueTypeVOList.map((item: any) => item.id));
            data.unshift(...issueTypeVOList);
          }
          return data;
        },
      }),
    },
  };
}
export default ContextOptionsDataSet;
