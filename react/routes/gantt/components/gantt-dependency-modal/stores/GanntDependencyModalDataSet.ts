import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import {
  DataSet,
} from 'choerodon-ui/pro';
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
const GanntDependencyModalDataSet = (editData?: any[]): DataSetProps => ({
  autoCreate: !editData?.length,
  autoQuery: false,
  data: editData,
  selection: false,
  fields: [
    {
      name: 'predecessorType',
      label: '依赖关系',
      required: true,
      type: 'string' as FieldType,
      textField: 'name',
      defaultValue: !editData?.length ? 'predecessor_fs' : undefined,
      valueField: 'valueCode',
      options: new DataSet({
        paging: false,
        autoQuery: true,
        transport: {
          read: {
            ...ganttApiConfig.loadIssueDependencyTypes(),
            transformResponse: (data) => sortBy(JSON.parse(data), (item) => getValueCodeSequence(item.valueCode)).map((item) => ({ ...item, name: escapeValueCode(item.valueCode) })),
          },
        },
      }),
    },
    {
      name: 'predecessorId', label: '工作项', required: true, type: 'string' as FieldType, multiple: true,
    },

  ],
  transport: {
  },
});
export default GanntDependencyModalDataSet;
