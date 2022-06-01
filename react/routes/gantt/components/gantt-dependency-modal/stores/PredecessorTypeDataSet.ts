import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { sortBy } from 'lodash';
import { ganttApiConfig } from '@/api';

function getValueCodeSequence(valueCode: string) {
  if (valueCode === 'predecessor_fs') {
    return 10;
  }
  return 30;
}
function escapeValueCode(valueCode: string) {
  const valueMap = {
    predecessor_fs: '完成-开始（FS）',
    predecessor_ff: '完成-完成（FF）',
    predecessor_ss: '开始-开始（SS）',
    predecessor_sf: '开始-完成（SF）',
  };
  return valueMap[valueCode as keyof typeof valueMap] || valueCode;
}
const PredecessorTypeDataSet = (projectId?: string): DataSetProps => ({
  paging: false,
  autoQuery: true,
  transport: {
    read: ({ data: queryData }) => ({
      ...ganttApiConfig.project(queryData?.projectId || projectId).loadIssueDependencyTypes(),
      transformResponse: (data) => sortBy(JSON.parse(data), (item) => getValueCodeSequence(item.valueCode)).map((item) => ({ ...item, name: escapeValueCode(item.valueCode) })),
    }),
  },
});
export default PredecessorTypeDataSet;
